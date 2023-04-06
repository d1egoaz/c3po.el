;;; c3po.el --- C3PO.el is an Emacs package that enables communication with the ChatGPT API. -*- lexical-binding: t -*-

;; Author: Diego Alvarez <c3po@diegoa.ca>
;; Keywords: c3po, chatgpt, openai
;; Package-Requires: ((emacs "27.1"))
;; Version: 1.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; 🤖 Meet c3po.el, the Emacs droid you’ve been looking for!
;; This package will take your workflow to a galaxy far, far away.  🌟
;; C3PO.el is an Emacs package for interacting with the ChatGPT API.
;; May the source be with you!
;;
;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.
;;
;;; Code:

(require 'diff-mode)
(require 'json)
(if (fboundp 'markdown-mode)
    (require 'markdown-mode) ; https://github.com/jrblevin/markdown-mode
  (message "`markdown-mode' package not found, some functionality may be limited."))
(require 'seq)
(require 'url)

;; It will be later redefined by url-http to avoid a warning during compilation.
(defvar url-http-end-of-headers)

(defvar c3po-api-key nil "The API key for the OpenAI API.")

(defvar c3po-buffer-name "*🤖C3PO🤖*" "The name of the C-3PO buffer.")

(defvar c3po-model "gpt-3.5-turbo" "The model for the OpenAI Chat API.")

(defvar c3po--last-used-persona nil "Last used persona to be used for replies.")

(defvar c3po-system-persona-prompts-alist
  '((corrector . (
                  :pre-processors nil
                  :post-processors (c3po-show-diff-post-processor c3po-copy-clipboard-post-processor)
                  :transient-key "c"
                  :system-prompt "
Please act as my grammar assistant.

I'll communicate with you in any language and you will correct and enhance the grammar in my text.

You should not modify contractions and avoid passive voice.
If I write:
user: I'll be fine
You shouldn't answer with:
assistant: I will be fine.
You should answer with:
assistant: I'll be fine.

I want you to only reply with the correction and nothing else, no explanations or questions.

If no corrections are needed, reply with the original text, example:
If I write:
user: Please act as my grammar assistant.
You shouldn't answer with:
assistant: No corrections needed, the sentence is grammatically correct.
You should answer with:
assistant: Please act as my grammar assistant.

The raw messages will be specified next, don't expect more commands:."))

    (developer . (
                  :pre-processors (c3po-add-to-buffer-pre-processor)
                  :post-processors (c3po-add-to-buffer-post-processor)
                  :transient-key "d"
                  :system-prompt "
You're a programming expert.
Your answers should be brief.
Your answers MUST be in full and well written markdown, code blocks MUST use the correct language tag."))

    (general . (
                :pre-processors (c3po-add-to-buffer-pre-processor)
                :post-processors (c3po-add-to-buffer-post-processor)
                :transient-key "g"
                :system-prompt "You are a helpful assistant. Keep it concise."))

    (rewriter . (
                 :pre-processors (c3po-add-to-buffer-pre-processor)
                 :post-processors (c3po-add-to-buffer-post-processor c3po-show-diff-post-processor)
                 :transient-key "r"
                 :system-prompt "
Please act as my writing assistant with a programming expertise.
I will speak to you in any language and you can enhance my text accordingly.
Maintain the meaning, use contractions, and avoid passive voice.
Your response MUST only include the improved text, without explanations or questions.
Keep it concise.
From now on, all my messages to you are intended to be enhanced")))
  "Alist of personas with a Plist of properties.
Call `c3po-make-persona-helper-functions' to have the helper functions created.")

(defvar c3po-command-history nil
  "History of commands for C3PO.")

(defvar c3po-chat-conversation '()
  "List of messages with personas user and assistant for the current chat.")

(defun create-c3po-dispatch ()
  "Creates a transient prefix dispatcher for each key in ALIST."
  (let ((items nil))
    (dolist (item c3po-system-persona-prompts-alist)
      (let* (
             (persona (car item))
             (persona-name (symbol-name persona))
             (cmd (intern (concat "c3po-" persona-name "-new-chat"))))
        (push (list (c3po-get-persona-property persona :transient-key) persona-name cmd) items)))
    (eval `(transient-define-prefix c3po-dispatch ()
             "C3PO transient dispatch."
             [["Actions"
               ,@items
               ]]))))

(defun c3po-add-new-persona-with-defaults-processors(persona transient-key system-prompt)
  (if (symbolp persona)
      (progn
        (add-to-list 'c3po-system-persona-prompts-alist
                     `(,persona . (
                                   :pre-processors (,'c3po-add-to-buffer-pre-processor)
                                   :post-processors (,'c3po-add-to-buffer-post-processor)
                                   :transient-key ,transient-key
                                   :system-prompt ,system-prompt)))
        ;; recreate functions using the macros
        (c3po-make-persona-helper-functions))
    (message "Please use a symbol for the persona key")))

(defun c3po-get-persona-property (persona prop)
  "Get property PROP for PERSONA."
  (plist-get (cdr (assoc persona c3po-system-persona-prompts-alist)) prop))

(defun c3po--apply-pre-processors (persona prompt)
  "Get the PERSONA processors and invoke the function, passing the PROMPT."
  (when-let ((processors (c3po-get-persona-property persona :pre-processors)))
    (seq-do (lambda (f) (funcall f persona prompt)) processors)))

(defun c3po--apply-post-processors (persona prompt result &rest args)
  "Get the PERSONA post-processors and invoke the function.
It pass to the function the PERSONA, PROMPT, RESULT, and ARGS."
  (when-let ((processors (c3po-get-persona-property persona :post-processors)))
    (seq-do (lambda (f) (funcall f persona prompt result args)) processors)))

(defun c3po--apply-post-processors-and-kill-region (persona prompt result &rest args)
  "Get the PERSONA post-processors and invoke the function.
It adds an additional processor to kill the current active region.
It pass to the function the PERSONA, PROMPT, RESULT, and ARGS."
  (save-window-excursion
    (when-let ((processors (append (c3po-get-persona-property persona :post-processors) '(c3po--kill-region-post-processor))))
      (seq-do (lambda (f) (funcall f persona prompt result args)) processors))))

(defun c3po-is-initial-system-message-p ()
  "Return t if the chat has only received an initial system message."
  (length= c3po-chat-conversation 1))

(defun c3po-add-to-buffer-pre-processor (persona prompt)
  "Pre-processor to add the PERSONA and PROMPT to the `c3po-buffer-name'."
  (c3po-append-result
   (if (c3po-is-initial-system-message-p)
       (format "\n# Chat (%s) - %s\n## 🙋‍♂️ Prompt\n%s\n" persona (format-time-string "%A, %e %B %Y %T %Z") prompt)
     (format "## 🙋‍♂️ Prompt\n%s\n" prompt))))

(defun c3po-add-to-buffer-post-processor (_persona _prompt result &rest _args)
  "Post-processor to add the RESULT to the `c3po-buffer-name'."
  (c3po-append-result (format "### 🤖 Answer\n%s\n" result))
  (pop-to-buffer c3po-buffer-name)
  (goto-char (point-max))
  (recenter))

(defun c3po--request-openai-api (callback &rest args)
  "Send chat messages request to OpenAI API, get result via CALLBACK.
Pass additional ARGS to the CALLBACK function."
  (interactive)
  (if (not c3po-api-key)
      (message "Please provide an OpenAI API key first.")
    (let* ((api-key c3po-api-key)
           (url "https://api.openai.com/v1/chat/completions")
           (model c3po-model)
           (url-request-method "POST")
           (url-request-extra-headers `(("Content-Type" . "application/json")
                                        ("Authorization" . ,(encode-coding-string(format "Bearer %s" api-key) 'utf-8))))
           (url-request-data (encode-coding-string
                              (json-encode `(:model ,model :messages ,c3po-chat-conversation))
                              'utf-8)))
      (url-retrieve url
                    #'c3po--extract-content-answer
                    (list callback args)))))

(defun c3po--extract-content-answer (_status callback &rest args)
  "Extract the last lines of a JSON string from a buffer.
Call user's CALLBACK with the result and passes the aditional ARGS."
  ;; url-http sets a marker named url-http-end-of-headers after retrieving the web content, allowing
  ;; us to skip the HTTP headers directly using this marker:
  (let* ((data (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max)))
         (json-string (decode-coding-string data 'utf-8))
         (json-object (json-read-from-string json-string))
         (message-content (aref (cdr (assoc 'choices json-object)) 0))
         (result (cdr (assoc 'content (cdr (assoc 'message message-content)))))
         (args2 (car args))
         (persona (cdr (assoc 'persona args2)))
         (prompt (cdr (assoc 'prompt args2)))
         (args (cdr (assoc 'args args2))))
    ;; post-processor
    ;; (when (string-suffix-p "\n" c3po--last-user-message)
    ;;   (setq content (concat content "\n")))
    (c3po--add-message "assistant" result)
    (apply callback persona prompt result args)))

(defun c3po-send-conversation (persona post-processors-fn &rest args)
  "Prepare the PROMPT for the PERSONA.
If POST-PROCESSORS-FN is nil it'll use `c3po--apply-post-processors'.
Pass ARGS to the `url-retrieve' function."
  (interactive)
  (catch 'my-tag
    (let ((prompt (if current-prefix-arg
                      (if (use-region-p)
                          (concat
                           (read-string (format "(%s)> Enter the prompt to act on the active region: " (symbol-name persona)) nil 'c3po-command-history)
                           "\n"
                           (buffer-substring-no-properties (region-beginning) (region-end)))
                        (progn
                          (message "When using universal-argument a region should be active.")
                          (throw 'my-tag nil)))
                    (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (read-string (format "(%s)> Enter the prompt: " (symbol-name persona)) nil 'c3po-command-history))
                    ))
          (post-fn (or post-processors-fn #'c3po--apply-post-processors)))
      (c3po--apply-pre-processors persona prompt)
      (c3po--add-message "user" prompt)
      (apply
       #'c3po--request-openai-api
       post-fn `((persona . ,persona) (prompt . ,prompt) (args . ,args))))))

(defun c3po-append-result (str)
  "Insert STR at the end of the c3po buffer."
  (let ((buf (get-buffer-create c3po-buffer-name)))
    (with-current-buffer buf
      (if (featurep 'markdown-mode)
          (gfm-mode)
        (text-mode))
      (goto-char (point-max))
      (insert (concat "\n" str))
      (goto-char (point-max)))))

(defun c3po--kill-region-post-processor (_persona prompt result &rest args)
  "Callback used to kill region with RESULT using ARGS.
Check PROMPT to validate if needs to add back the new line."
  ;; Adds back the final new line if the prompt had it.
  (when (string-suffix-p "\n" prompt)
    (setq result (concat result "\n")))

  (let* ((arguments (car args))
         (buf (nth 0 arguments)) ; gets buffer name
         (beg (nth 1 arguments)) ; region beg
         (end (nth 2 arguments))) ; region end
    (with-current-buffer buf
      (save-excursion
        (kill-region beg end)
        ;; (delete-region beg end)
        (goto-char beg)
        (insert result))
      (keyboard-escape-quit))))

(defun c3po-send-conversation-and-kill-region (persona)
  "Setup c3po to start a `c3po-send-conversation' with PERSONA.
And result will be used by `c3po--callback-kill-region'."
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (c3po-send-conversation persona
                                #'c3po--apply-post-processors-and-kill-region
                                (buffer-name) ; here is where the additional args are passed
                                beg
                                end))
    (message "No region selected or region is empty")))

(defun c3po-show-diff-post-processor (_persona prompt result &rest _args)
  "Callback to show diff from PROMPT vs RESULT (tail of ARGS)."
  ;; Adds back the final new line if the prompt had it.
  (when (string-suffix-p "\n" prompt)
    (setq result (concat result "\n")))
  (c3po--diff-strings prompt result)
  (pop-to-buffer "*Diff*"))

(defun c3po-copy-clipboard-post-processor (_persona prompt result &rest _args)
  "Post-processor to copy the RESULT to the kill ring.
Check if needs to fix RESULT new lines given the PROMPT."
  ;; Adds back the final new line if the prompt had it.
  (if (string-suffix-p "\n" prompt)
      (kill-new (concat result "\n"))
    (kill-new result)))

(defmacro !c3po--make-chat (persona)
  "Macro to create functions chats for each PERSONA."
  `(defun ,(intern (concat "c3po-" (symbol-name persona) "-new-chat")) ()
     ,(format "Interact with the Chat API using the persona %s." (symbol-name persona))
     (interactive)
     (c3po-new-chat ',persona)
     (c3po-send-conversation ',persona nil)))

(defmacro !c3po--make-kill-region-chat (persona)
  "Macro to create functions to kill regions chats for each PERSONA."
  `(defun ,(intern (concat "c3po-" (symbol-name persona) "-new-chat-kill-region")) ()
     ,(format "Interact with the Chat API using the persona %s. Also kill the region with the result." (symbol-name persona))
     (interactive)
     (c3po-new-chat ',persona)
     (c3po-send-conversation-and-kill-region ',persona)))

(defun c3po-make-persona-helper-functions ()
  "Create all the persona chats and kill-region chats.
Example: c3po-corrector-chat, c3po-corrector-chat-kill-region, etc."
  (dolist (element c3po-system-persona-prompts-alist)
    (let ((persona (car element)))
      (eval `(!c3po--make-chat ,(car element)))
      (eval `(!c3po--make-kill-region-chat ,(car element)))))
  (create-c3po-dispatch))

(c3po-make-persona-helper-functions)

(defun c3po--diff-strings (str1 str2)
  "Compare two strings (STR1 and STR2) and return the result."
  (let ((buf1 (generate-new-buffer " *c3po-diff-str1*"))
        (buf2 (generate-new-buffer " *c3po-diff-str2*"))
        (diff-output nil))
    (with-current-buffer buf1
      (insert str1))
    (with-current-buffer buf2
      (insert str2))
    ;; Perform the diff and store it in a new buffer
    (with-temp-buffer
      (diff buf1 buf2 "-U0") ; -U0: Unidiff with 0 lines of context
      (diff-mode)
      (diff-refine-hunk)) ; to highlight single character changes
    (kill-buffer buf1)
    (kill-buffer buf2)
    diff-output))

(defun c3po-explain-code ()
  "Explain the code for the selected region, or prompt the user for input."
  (interactive)
  (c3po-send-conversation
   'developer
   (lambda (prompt)
     (format "Explain the following code, be concise:\n```%s\n%s```" (c3po--get-buffer-mode-as-tag) prompt))))

(defun c3po--get-buffer-mode-as-tag ()
  "Get buffer mode as a string to be used as a tag for a markdown code block."
  (let ((str (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)) ))
    (if (string-suffix-p "-ts" str) ;; for the new *-ts-modes
        (substring str 0 (- (length str) 3))
      str)))

(defun c3po--add-message (role content)
  "Add a message with given ROLE and CONTENT to the chat message alist."
  (setq c3po-chat-conversation (append c3po-chat-conversation `((("role" . ,role) ("content" . ,content))))))

(defun c3po-new-chat (persona)
  "Reset the chat conversation and set a new chat using PERSONA."
  (setq c3po-chat-conversation '())
  (c3po--add-message "system" (c3po-get-persona-property persona :system-prompt))
  (setq c3po--last-used-persona persona))

(defun c3po-reply ()
  "Reply with a message and submit the new conversation."
  (interactive)
  (c3po-send-conversation c3po--last-used-persona nil))

(provide 'c3po)
;;; c3po.el ends here

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
;; This package will take your workflow to a galaxy far, far away. 🌟
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
(require 'markdown-mode) ;; https://github.com/jrblevin/markdown-mode
(require 'seq)
(require 'url)

;; It will be later redefined by url-http to avoid a warning during compilation.
(defvar url-http-end-of-headers)

(defvar c3po-api-key nil "The API key for the OpenAI API.")

(defvar c3po-buffer-name "*🤖C3PO🤖*" "The name of the C-3PO buffer.")

(defvar c3po-model "gpt-3.5-turbo" "The model for the OpenAI Chat API.")

(defvar c3po-system-persona-prompts-alist-plist
  `((corrector . (:pre-processors () :post-processors (,#'c3po--show-diff-post-processor ,#'c3po--copy-clipboard-post-processor) :system-prompt "
Please act as my grammar assistant.
I will communicate with you in any language and you will correct and enhance the grammar in my text.
You may use contractions and avoid passive voice.
I want you to only reply with the correction and nothing else, no explanations or questions.

If no corrections are needed, reply with the original text, example:
If I write:
user: Please act as my grammar assistant.
You shouldn't answer with:
assistant: No corrections needed, the sentence is grammatically correct.
You should answer with:
assistant: Please act as my grammar assistant.

The raw messages will be specified next, don't expect more commands:."))

    (developer . (:pre-processors (,#'c3po--add-to-buffer-pre-processor) :post-processors (,#'c3po--add-to-buffer-post-processor) :system-prompt "
You're a programming expert.
Your answers should be brief.
Your answers MUST be in full and well written markdown, code blocks MUST use the correct language tag."))

    (general . (:pre-processors (,#'c3po--add-to-buffer-pre-processor) :post-processors (,#'c3po--add-to-buffer-post-processor)
                                :system-prompt "You are a helpful assistant. Keep it concise."))

    (rewriter . (:pre-processors (,#'c3po--add-to-buffer-pre-processor) :post-processors (,#'c3po--add-to-buffer-post-processor ,'c3po--show-diff-post-processor)
                                 :system-prompt "
Please act as my writing assistant with a programming expertise.
I will speak to you in any language and you can enhance my text accordingly.
Maintain the meaning, use contractions, and avoid passive voice.
Your response MUST only include the improved text, without explanations or questions.
Keep it concise.
From now on, all my messages to you are intended to be enhanced"))

    (summarizer . (:pre-processors (,#'c3po--add-to-buffer-pre-processor) :post-processors (,#'c3po--add-to-buffer-post-processor)
                                   :system-prompt "Please act as my writing assistant with a programming expertise. tl;dr")))
  "Alist of personas with a Plist of properties.
Call `c3po-make-persona-helper-functions' to have the helper functions created.")

(defun c3po-get-property-for-persona (persona prop)
  "Get property PROP for PERSONA."
  (plist-get (cdr (assoc persona c3po-system-persona-prompts-alist-plist)) prop))

(defun c3po--apply-pre-processors (persona prompt)
  "Get the PERSONA processors and invoke the function, passing the PROMPT."
  (when-let ((processors (c3po-get-property-for-persona persona :pre-processors)))
    (seq-do (lambda (f) (funcall f persona prompt)) processors)))

;; TODO: what about args?
(defun c3po--apply-post-processors (persona prompt result &rest _args)
  "Get the PERSONA post-processors and invoke the function, passing the PROMPT and RESULT."
  (when-let ((processors (c3po-get-property-for-persona persona :post-processors)))
    (seq-do (lambda (f) (funcall f persona prompt result)) processors)))

(defun c3po--apply-post-processors-with-replace (persona prompt result &rest args)
  "Get the PERSONA post-processors and invoke the function, passing the PROMPT and RESULT."
  (save-window-excursion
    (when-let ((processors (append (c3po-get-property-for-persona persona :post-processors) '(c3po--replace-region-post-processor))))
      (seq-do (lambda (f) (funcall f persona prompt result args)) processors))))

(defun c3po--add-to-buffer-pre-processor (persona prompt)
  (c3po-append-result
   (format "\n# New Chat (%s) - %s\n## 🙋‍♂️ Prompt\n%s\n" persona (format-time-string "%A, %e %B %Y %T %Z") prompt))
  prompt)

(defun c3po--add-to-buffer-post-processor (_persona _prompt result &rest _args)
  (c3po-append-result (format "### 🤖 Answer\n%s\n" result))
  (pop-to-buffer c3po-buffer-name)
  (goto-char (point-max))
  (recenter))

(defvar c3po-command-history nil
  "History of commands for C3PO.")

(defvar c3po--chat-messages '()
  "List of messages with personas user and assistant for the current chat.")

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
                              (json-encode `(:model ,model :messages ,c3po--chat-messages))
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

(defun c3po--chat (persona post-processors-fn &rest args)
  "If POST-PROCESSORS-FN is nil it'll use `c3po--apply-post-processors'."
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
      (c3po-new-chat)
      (c3po--add-message "system" (c3po-get-property-for-persona persona :system-prompt))
      (c3po--add-message "user" prompt)
      (apply
       #'c3po--request-openai-api
       post-fn `((persona . ,persona) (prompt . ,prompt) (args . ,args))))))

(defun c3po-append-result (str)
  "Insert STR at the end of the c3po buffer."
  (save-window-excursion
    (let ((buf (get-buffer-create c3po-buffer-name)))
      (with-current-buffer buf
        (gfm-mode)
        (goto-char (point-max))
        (insert (concat "\n" str))
        (goto-char (point-max))))))

(defun c3po--replace-region-post-processor (persona prompt result &rest args)
  "Callback used to replace region with RESULT using ARGS."
  (let* ((arguments (car args))
         (buf (nth 0 arguments)) ; gets buffer name
         (beg (nth 1 arguments)) ; region beg
         (end (nth 2 arguments))) ; region end
    (with-current-buffer buf
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert result))
      (keyboard-escape-quit))))

(defun c3po--chat-and-replace-region (persona)
  "Setup c3po to start a `c3po--chat' with PERSONA.
And result will be used by `c3po--callback-replace-region'."
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (c3po--chat persona
                    #'c3po--apply-post-processors-with-replace
                    (buffer-name) ; here is where the additional args are passed
                    beg
                    end))
    (message "No region selected or region is empty")))

(defun c3po--show-diff-post-processor (_persona prompt result &rest _args)
  "Callback to show diff from PROMPT vs RESULT (tail of ARGS)."
  (c3po--diff-strings prompt result)
  (pop-to-buffer "*Diff*"))

(defun c3po--copy-clipboard-post-processor (_persona _prompt result &rest _args)
  (kill-new result))

(defmacro !c3po--make-chat (persona)
  "Macro to create functions chats for each PERSONA."
  `(defun ,(intern (concat "c3po-" (symbol-name persona) "-chat")) ()
     ,(format "Interact with the Chat API using the persona %s." (symbol-name persona))
     (interactive)
     (c3po--chat ',persona nil)))

(defmacro !c3po--make-replace-region-chat (persona)
  "Macro to create functions replacement chats for each PERSONA."
  `(defun ,(intern (concat "c3po-" (symbol-name persona) "-chat-replace-region")) ()
     ,(format "Interact with the Chat API using the persona %s.
Also replaces the region with the result" (symbol-name persona))
     (interactive)
     (c3po--chat-and-replace-region ',persona)))

(defun c3po-make-persona-helper-functions ()
  "Create all the persona chats and replace-region chats.
Example: c3po-corrector-chat, c3po-corrector-chat-replace-region, etc."
  (dolist (element c3po-system-persona-prompts-alist-plist)
    (progn
      (eval `(!c3po--make-chat ,(car element)))
      (eval `(!c3po--make-replace-region-chat ,(car element))))))
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
    ;; not interested for now, as I can use the diff buffer to show diffs
    ;; additional (buffer-string) is only returning the first line :/
    ;; (with-current-buffer "*Diff*"
    ;;   (setq diff-output (buffer-string)))
    (kill-buffer buf1)
    (kill-buffer buf2)
    diff-output))

(defun c3po-explain-code ()
  "Explain the code for the selected region, or prompt the user for input."
  (interactive)
  (c3po--chat
   'developer
   (lambda (prompt)
     (format "Explain the following code, be concise:\n```%s\n%s```" (c3po--get-buffer-mode-as-tag) prompt))))

(defun c3po--get-buffer-mode-as-tag ()
  "Get buffer mode as a string to be used as a tag for a markdown code block."
  (let ((str (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)) ))
    (if (string-suffix-p "-ts" str) ;; for the new *-ts-modes
        (substring str 0 (- (length str) 3))
      str)))

;;; Chat support

(defun c3po--add-message (role content)
  "Add a message with given ROLE and CONTENT to the chat message alist."
  (setq c3po--chat-messages (append c3po--chat-messages `((("role" . ,role) ("content" . ,content))))))

(defun c3po-new-chat ()
  "Reset the chat message list to an empty list."
  (setq c3po--chat-messages '()))

(defun c3po-reply ()
  "Reply with a message and submit the information."
  (interactive)
  (let ((prompt (read-string "Enter your reply prompt: " nil 'c3po-command-history)))
    (c3po--add-message "user" prompt)
    (c3po-append-result (format "#### 🙋‍♂️ Reply\n%s\n" prompt))
    (c3po--request-openai-api (lambda (result &rest _args)
                                (c3po-append-result (format "##### 🤖 Answer\n%s\n" result))
                                (pop-to-buffer c3po-buffer-name)
                                (goto-char (point-max))
                                (recenter)))))

(provide 'c3po)
;;; c3po.el ends here

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

(defvar c3po-system-persona-prompts-alist
  (rewriter . "")
  (summarizer . ""))
"Alist of known system prompts.
Call `c3po-make-persona-helper-functions' to have the helper functions created.")

(defvar c3po-system-persona-prompts-alist-plist
  `((corrector . (:pre-processors () :post-processors (,#'c3po--show-diff-postprocessor) :system-prompt "Please act as my grammar assistant. I will communicate with you in any language and you will correct and enhance the grammar in my text. You may use contractions and avoid passive voice. I want you to only reply with the correction and nothing else, no explanations or questions. From now on, only answer with the correction and no questions or additional messages."))
    (developer . (:pre-processors (,#'c3po--add-to-buffer-preprocessor) :post-processors () :system-prompt "You're a programming expert. Your answers should be brief. Your answers MUST be in full and well written markdown, code blocks MUST use the correct language tag."))
    (general . (:pre-processors (,#'c3po--add-to-buffer-preprocessor) :post-processors (,#'c3po--add-to-buffer-postprocessor) :system-prompt "You are a helpful assistant. Keep it concise."))
    (rewriter . (:pre-processors (,#'c3po--add-to-buffer-preprocessor) :post-processors () :system-prompt "Please act as my writing assistant with a programming expertise. I will speak to you in any language and you can enhance my text accordingly. Maintain the meaning, use contractions, and avoid passive voice. Your response MUST only include the improved text, without explanations or questions. Keep it concise. From now on, all my messages to you are intended to be enhanced"))
    (summarizer . (:pre-processors (,#'c3po--add-to-buffer-preprocessor) :post-processors () :system-prompt "Please act as my writing assistant with a programming expertise. tl;dr")))
  "Alist of personas with a Plist of properties.
Call `c3po-make-persona-helper-functions' to have the helper functions created.")

(defun c3po-get-property-for-persona (persona prop)
  "Get property PROP for PERSONA."
  (plist-get (cdr (assoc persona c3po-system-persona-prompts-alist-plist)) prop))

(defun c3po--apply-preprocessors (persona prompt)
  (if-let ((transformers (c3po-get-property-for-persona persona :pre-processors)))
      (seq-reduce (lambda (p f) (funcall f persona p)) transformers prompt)
    prompt))

(defun c3po--apply-postprocessors (persona prompt result &rest _args)
  (if-let ((transformers (c3po-get-property-for-persona persona :post-processors)))
      (seq-reduce (lambda (p f) (funcall f persona p result)) transformers prompt)
    result))

(defun c3po--add-to-buffer-preprocessor (persona prompt)
  (c3po-append-result
   (format "\n# New Chat (%s) - %s\n## 🙋‍♂️ Prompt\n%s\n" persona (format-time-string "%A, %e %B %Y %T %Z") prompt))
  prompt)

(defun c3po--add-to-buffer-postprocessor (_persona _prompt result)
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
  (message ">>> args: %S" args)
  (let* ((data (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max)))
         (json-string (decode-coding-string data 'utf-8))
         (json-object (json-read-from-string json-string))
         (message-content (aref (cdr (assoc 'choices json-object)) 0))
         (result (cdr (assoc 'content (cdr (assoc 'message message-content)))))
         (args2 (car args))
         (persona (cdr (assoc 'persona args2)))
         (prompt (cdr (assoc 'prompt args2)))
         (args (cdr (assoc 'args args2))))
    (message ">>> args2: %S %S %S" persona prompt args)
    ;; postprocessor
    ;; (when (string-suffix-p "\n" c3po--last-user-message)
    ;;   (setq content (concat content "\n")))
    (c3po--add-message "assistant" result)
    (apply callback persona prompt result args)))

(defun c3po--chat (persona &rest args)
  (interactive)
  (let* ((prompt (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (read-string (format "Enter your prompt (%s): " (symbol-name persona)) nil 'c3po-command-history)))
         (prompt (c3po--apply-preprocessors persona prompt)))
    (c3po-new-chat)
    (c3po--add-message "system" (c3po-get-property-for-persona persona :system-prompt))
    (c3po--add-message "user" prompt)
    (apply
     #'c3po--request-openai-api
     #'c3po--apply-postprocessors `((persona . ,persona) (prompt . ,prompt) (args . ,args)))))

(defun c3po-append-result (str)
  "Insert STR at the end of the c3po buffer."
  (save-window-excursion
    (let ((buf (get-buffer-create c3po-buffer-name)))
      (with-current-buffer buf
        (gfm-mode)
        (goto-char (point-max))
        (insert (concat "\n" str))
        (goto-char (point-max))))))

(defun c3po--callback-replace-region (result &rest args)
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
                    nil
                    #'c3po--callback-replace-region
                    (buffer-name) ; here is where the additional args are passed
                    beg
                    end))
    (message "No region selected or region is empty")))

(defun c3po--show-diff-postprocessor (_persona prompt result)
  "Callback to show diff from PROMPT vs RESULT (tail of ARGS)."
  (c3po--diff-strings prompt result)
  (pop-to-buffer "*Diff*"))

(defmacro !c3po--make-chat (persona)
  "Macro to create functions chats for each PERSONA."
  `(defun ,(intern (concat "c3po-" (symbol-name persona) "-chat")) ()
     ,(format "Interact with the Chat API using the persona %s." (symbol-name persona))
     (interactive)
     (c3po--chat ',persona)))

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
  (dolist (element c3po-system-persona-prompts-alist)
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
  "Explain the code for the region or prompt user for input."
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

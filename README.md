# c3p0.el

<img src="./c3p-o.png" height="30%" width="30%">

**C3PO.el** is an Emacs package for interacting with the ChatGPT API. This package is named after the
famous protocol droid from Star Wars, who was known for his ability to communicate and translate
languages. Similarly, C3PO.el provides an AI-powered chatbot interface that allows users to interact
with the ChatGPT API through Emacs.

LOL, Obviously, above was written by ChatGPT.

This is my first package, it's what I've been using daily. I just wanted to share my workflow and improve my Emacs Lisp skills.

## Some Details
- ChatGPT API is called using only emacs built-in packages. I'm using Emacs 30+.
- Except for the region replacement commands, all prompts and responses are displayed in the C3PO buffer.
- Commands without a specified region will prompt the user for input.
- All commands support a conversation style, so you can reply with additional prompts.
  The session messages are stored in memory, so you can modify the result buffer without affecting the conversation.
- Some commands by default initiate a new conversation session:
  - `c3po-dev-chat`, `c3po-chat`, `c3po-correct-grammar`, `c3po-correct-grammar-and-replace`, `c3po-rewrite-text`, `c3po-rewrite-and-replace`, `c3po-explain-code`, and `c3po-summarize`.
  - You can keep the conversation going by using `c3po-reply`.
- The `dev` and writter roles/personas system prompts can be configurable. I'll probably add something later.

## Examples

### Using a dev persona
`c3po-dev-chat` and `c3po-reply`.

https://user-images.githubusercontent.com/950087/226485879-0a2b032b-fc18-47f6-bdc8-eb7c6f4e6506.mp4

### Using a writter persona / general chat
`c3po-chat` and `c3po-reply`.

https://user-images.githubusercontent.com/950087/226485945-1b7d4adf-8f1f-48ad-9bd8-83385e816d14.mp4

### Correct grammar, and show detailed changes in a diff buffer
`c3po-correct-grammar` and `c3po-reply`.
This one shows a `diff-mode` buffer, and uses `diff-refine-hunk` to show a finer detail of the changes. As my goal is to improve my grammar and observe differences more effectively.

https://user-images.githubusercontent.com/950087/226486071-7b7c0b7b-25d5-4f6f-a5b9-93c06e8cb0d1.mp4

### Correct grammar in place and replace selected region
`c3po-correct-grammar-and-replace`.

https://user-images.githubusercontent.com/950087/226486611-ed37a95b-19dd-4976-a322-523668b64364.mp4

### Rewrite text, and rewrite text and replace current region
`c3po-rewrite-text`, `c3po-rewrite-and-replace`, and `c3po-reply`.

https://user-images.githubusercontent.com/950087/226486652-7fd99b9e-1135-46b3-ac43-f20cdd2ad6a6.mp4

### Explain code
`c3po-explain-code`.

https://user-images.githubusercontent.com/950087/226486887-9ad11aab-0934-49b2-ac44-3a6fb29992bc.mp4

### Summary a text
`c3po-summarize` and `c3po-reply`.

https://user-images.githubusercontent.com/950087/226487018-99ee25b2-e04f-464e-a0de-3a287a453df1.mp4

## Installation

Please provide an API key in `c3po-api-key`.

```emacs-lisp
(use-package c3po
  :straight (:host github :repo "d1egoaz/c3po.el")
  :config
  (setq c3po-api-key "<chose-your-own-adventure>"))
```

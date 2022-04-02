(add-to-list 'default-frame-alist '(undecorated . t))
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; Set the threshold to very high to not garbage collect
(setq gc-cons-threshold most-positive-fixnum)

;; Restore `gc-cons-threshold' to a reasonable value after init
(add-hook 'emacs-startup-hook  ; Hook that runs after init files are loaded
          (lambda ()
            ;; Set a reasonable value that won't hang Emacs or won't increase ram usage
            (setq gc-cons-threshold (* 50 1000 1000)))
          100)  ; Make sure this function will run at the end of `emacs-startup-hook's.


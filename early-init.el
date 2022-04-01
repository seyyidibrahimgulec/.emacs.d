(add-to-list 'default-frame-alist '(undecorated . t))
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))


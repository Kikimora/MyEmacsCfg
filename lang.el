;; Использовать окружение UTF-8
(set-language-environment 'UTF-8)
;; UTF-8 для вывода на экран
(set-terminal-coding-system 'utf-8)
;; UTF-8 для ввода с клавиатуры
(set-keyboard-coding-system 'utf-8)
;; UTF-8 для работы с буфером обмена X (не работает в emacs 21!)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; Необходима поддержка кодировок cp866 и cp1251
(codepage-setup 1251)
(define-coding-system-alias 'windows-1251 'cp1251)
(codepage-setup 866)
;; Установки автоопределения кодировок
;; prefer-coding-system помещает кодировку в НАЧАЛО списка предпочитаемых кодировок
;; Поэтому в данном случае первой будет определяться utf-8-unix
(prefer-coding-system 'cp866)
(prefer-coding-system 'koi8-r-unix)
(prefer-coding-system 'windows-1251-dos)
(prefer-coding-system 'utf-8-unix)
;; Клавиатурная раскладка "как в Windows" (не работает в emacs 21!)
(setq default-input-method 'russian-computer)

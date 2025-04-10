(defvar md/big-frame-alist
  '((width . 175) (height . 46) (left . 10) (top . 30)))
(defvar md/small-frame-alist
  '((width . 85) (height . 25) (left . 10) (top . 30)))
(defvar md/narrow-frame-alist
  '((width . 85) (height . 35) (left . 10) (top . 30)))

(setq default-frame-alist md/big-frame-alist)

(add-to-list 'default-frame-alist '(z-group . above))

(setq initial-frame-alist default-frame-alist)

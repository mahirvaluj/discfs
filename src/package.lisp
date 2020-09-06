(defpackage :discfs
  (:use :cl :lispcord :ironclad :split-sequence :qbase64 :str)
  (:shadow :get)
  (:export :mount-channel :get :put :del))

;nyquist plug-in
;version 1
;type process
;name "Subliminal..."
;action "Subliminal..."
;control carrier "Carrier" real "Hz" 17500 14000 20000

;;Switch to sound smapling rate.
(setf *nyquist-srate* (/ *sound-srate* 2.0))

;;Set the carrier frequency
(setf carrier (max 14000 (min carrier (- *nyquist-srate* 3000))))

;; We have two Nyquist frequencies, carrier/2 and *sound-srate*/2.
;; The CUTOFF is the maximum allowed frequency in the modulator.
;; It must not be greater than carrier/2, but also not greater than
;; the difference between the carrier and *sound-srate*/2, because
;; otherwise the modulated carrier aliases.

(setf cutoff (min (/ carrier 2.0) (- *nyquist-srate* carrier)))

(defun cut (function sound frequency)
  (dotimes (ignore 10 sound)
    (setf sound (funcall function sound frequency))))

(defun subliminal (sound)
  (let ((result (mult 2 (cut #'lowpass8 (hp sound 80) cutoff)
                        (hzosc carrier))))
    (cut #'highpass8 result carrier)))

(if (< *sound-srate* 44100)
    (princ "The track sample frequency must be minimum 44100Hz.")
    (multichan-expand #'subliminal s))

(ns clj-arduino.ov7670-img
  (:use clj-arduino.ov7670)
  (:require [clojure.java.io :as io]
            [clodiuno.core :refer [close]])
  (:import [java.awt.image BufferedImage DataBufferByte]
           [javax.imageio ImageIO]))

(defn gray-image
  "for testing"
  [path shade]
  (let [arr (into-array Byte/TYPE (repeat (* 144 88) (byte shade)))
        image (BufferedImage. 88 144 BufferedImage/TYPE_BYTE_GRAY)
        data (-> image .getRaster .getDataBuffer .getData)]
    (System/arraycopy arr 0 data 0 (* 144 88))
    (ImageIO/write image, "jpg", (io/file path))))

(comment 
  (gray-image "resources/img_0.jpg" 0) ;; black
  (gray-image "resources/img_8.jpg" 8) ;; black
  (gray-image "resources/img_64.jpg" 64) ;; dark gray
  (gray-image "resources/img_127.jpg" 127) ;; medium gray
  (gray-image "resources/img_-128.jpg" -128) ;; medium gray
  (gray-image "resources/img_-64.jpg" -64) ;; light gray
  (gray-image "resources/img_-8.jpg" -8) ;; off white
  )

(defn- image-size
  "return width, height"
  [image]
  [(alength (aget image 0)) (alength image)])

(defn- flatten-img
  "flatten 2 dimensional image into single dim byte array"
  [image]
  (let [[w h] (image-size image)]
    (loop [i h
           bytez []]
      (if (zero? i) (->> bytez flatten (into-array Byte/TYPE))
          (recur (dec i)
                 (conj bytez  
                       (loop [j w
                              row []]
                         (if (zero? j) row
                             (recur (dec j) (conj row (aget image (dec i) (dec j))))))))))))


(defn to-jpg
  "generate a jpg from the image"
  [image path]
  (let [[w h] (image-size image)
        image2 (BufferedImage. w h BufferedImage/TYPE_BYTE_GRAY)
        data (-> image2 .getRaster .getDataBuffer .getData)
        bytes (flatten-img image)]
    (System/arraycopy bytes 0 data 0 (alength bytes))
    (ImageIO/write image2, "jpg", (io/file path))))

(comment
l  @accum
  (to-jpg (:image @accum) "resources/ov7670_8mhz_dark.jpg")
  (to-jpg (:image @accum) "resources/ov7670_8mhz_light.jpg")
  (to-jpg (:image @accum) "resources/ov7670_1mhz_dark.jpg")
  (to-jpg (:image @accum) "resources/ov7670_1mhz_light.jpg")
  (open-board)
  (initialize)
  (close board)
  (cmd :capture-image)
  (cmd :some-pinc-d)
  (cmd :clock-at-1mhz)
  (cmd :clock-at-8mhz)
  (cmd :vsync-timing)
  (cmd :count-pixels)
  (cmd :pckl-timing)
  (cmd :test)

  )


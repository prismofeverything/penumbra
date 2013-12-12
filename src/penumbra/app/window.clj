;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns penumbra.app.window
  (:use [penumbra.opengl]
        [penumbra.utils :only (-?>)])
  (:require [penumbra.opengl
              [texture :as texture]
              [context :as context]]
            [penumbra.text :as text]
            [penumbra.app.event :as event]
            [penumbra.app.core :as app])
  (:import [org.lwjgl.opengl Display PixelFormat]
           [org.newdawn.slick.opengl InternalTextureLoader TextureImpl]
           [java.awt Frame Canvas GridLayout Color]
           [java.awt.event WindowAdapter]))

;;;

(defprotocol Window
  (display-modes [w] "Returns all display modes supported by the display device.")
  (display-mode [w] "Returns the current display mode.")
  (display-mode! [window w h] [w mode] "Sets the display mode.")
  (title! [w title] "Sets the title of the application.")
  (size [w] "Returns the current size of the application.")
  (resized? [w] "Returns true if application was resized since handle-resize! was last called.")
  (invalidated? [w] "Returns true if the window is invalidated by the operating system.")
  (close? [w] "Returns true if the user has requested it be closed.")
  (process! [w] "Processes all messages from the operating system.")
  (update! [w] "Swaps the buffers.")
  (handle-resize! [w] "Handles any resize events.  If there wasn't a resizing, this is a no-op.")
  (init! [w] "Initializes the window.")
  (destroy! [w] "Destroys the window.")
  (vsync! [w flag] "Toggles vertical sync.")
  (fullscreen! [w flag] "Toggles fullscreen mode."))

;;;

(defn- transform-display-mode [m]
  {:resolution [(.getWidth m) (.getHeight m)]
   :bpp (.getBitsPerPixel m)
   :fullscreen (.isFullscreenCapable m)
   :mode m})

(defn reshape
  [app size]
  (let [[w h] size]
    (viewport 0 0 w h)))

(defn create-fixed-window [app]
  (reify
    Window
    (vsync! [_ flag] (Display/setVSyncEnabled flag))
    (fullscreen! [this flag] 
      (Display/setFullscreen flag)
      (reshape app (size this)))
    (title! [_ title] (Display/setTitle title))
    (display-modes [_] (map transform-display-mode (Display/getAvailableDisplayModes)))
    (display-mode [_] (transform-display-mode (Display/getDisplayMode)))
    (display-mode! [_ mode] (Display/setDisplayMode (:mode mode)))
    (display-mode! [this w h]
      (let [max-bpp (apply max (map :bpp (display-modes this)))]
        (->> (display-modes this)
             (filter #(= max-bpp (:bpp %)))
             (sort-by #(Math/abs (apply * (map - [w h] (:resolution %)))))
             first
             (display-mode! this))))
    (size [this]
      (let [w (Display/getWidth) h (Display/getHeight)]
        [w h]))
    (resized? [this] (Display/wasResized))
    (invalidated? [_] (Display/isDirty))
    (close? [_] (try
                  (Display/isCloseRequested)
                  (catch Exception e
                    true)))
    (update! [_] (Display/update))
    (process! [_] (Display/processMessages))
    (handle-resize! [this]
      (when (resized? this)
        (let [[w h] (size this)]
          (viewport 0 0 w h)
          (event/publish! app :reshape [(Display/getX) (Display/getY) w h]))))
    (init! [this]
      (when-not (Display/isCreated)
        (Display/setParent nil)
        (Display/create (PixelFormat.))
        (Display/setResizable true)
        (display-mode! this 800 600))
      (-> (InternalTextureLoader/get) .clear)
      (TextureImpl/bindNone)
      (let [[w h] (size this)]
        (viewport (Display/getX) (Display/getY) w h)))
    (destroy! [_]
      (-> (InternalTextureLoader/get) .clear)
      (context/destroy)
      (Display/destroy))))

(defmacro with-window [window & body]
  `(context/with-context nil
     (binding [*window* ~window]
       ~@body)))


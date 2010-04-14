;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.opengl.squares
  (:use [penumbra opengl compute])
  (:require [penumbra.app :as app]))

(defn init [state]

  (defpipeline shader
    :vertex {:position (* :model-view-projection-matrix :vertex)
             position (float3 :vertex)}
    :fragment (color3 (-> position .x abs)))

  state)

(defn reshape [_ state]
  (ortho-view -1 1 -1 1 -1 1)
  state)

(defn display [_ state]
  (enable :texture-rectangle)
  (scale 0.9 0.9)
  (blit!
   (with-pipeline shader [(app/size)]
     (clear)
     (draw-quads
      (vertex -1 -1) (vertex 1 -1)
      (vertex  1 1) (vertex -1 1)))))

(defn start []
  (app/start {:init init, :reshape reshape, :display display} {}))
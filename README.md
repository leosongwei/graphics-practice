Graphics Practice
=================

Computer graphics practice, written in OpenGL and Common Lisp

Requirement:

* SBCL
* SDL2
* cl-png
  - libpng12
* Linux

Load files: `sbcl --load triangle.lisp`

* `triangle.lisp`
  - Basic draw array
  - <img src="img/triangle.png" width=200 />
* `quad.lisp`
  - Draw with EBO
  - <img src="img/quad.png" width=200 />
* `color.lisp`
  - The color of the quad changing over time.
  - Passing color value to Shader program using uniform
  - 2 different Shader programs
  - <img src="img/color.png" width=200 />
* `texture.lisp`
  - load texture
  - <img src="img/texture.png" width=200/>
* `perspective.lisp`
  - load obj model
  - projection
  - <img src="img/perspective.png" width=200/>
* `light.lisp`
  - ambient, diffuse, specular
  - <img src="img/light.png" width=200/>

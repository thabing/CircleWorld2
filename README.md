# CircleWorld2
Playground for DirectX, Games, Pathfinding, Collision Detection, ...

This program renders a large 2-D playfield composed of circles and polygons which are defined using a subset of SVG XML. 

As you move the avatar around, areas which would be obscured from the avatars sight by environmental objects are shaded.

Collision detection is optimized by partioning the space using quadtrees.

The open source VelcroPhysics (fomerly Farseer) engine is used mostly for its polygon decomposition algorithms which can turn concave polygons 
into convex polygons in order to support the collision detection algorithm.

Most of the graphics utilize the System.Drawing.Drawing2D libraries.  

Keyboard control is provided by the DirectX DirectInput libraries as wrapped by the SlimDX libraries.

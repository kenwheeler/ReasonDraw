type floatPointType = {x: float, y: float};

type intPointType = {x: int, y: int};

type drawPointType = {x: int, y: int, size: int};

type wCoordType =
  | WCoord floatPointType;

type gCoordType =
  | GCoord intPointType;

type gameStateType = {
  mutable drawables: list drawPointType,
  mutable brushSize: int,
};

let module Color = {
  let black = (0., 0., 0.);
  let red = (1., 0., 0.);
  let yellow = (0.97, 0.7, 0.);
  let brightYellow = (0.985, 0.88, 0.3);
  let brown = (0.28, 0.21, 0.02);
  let grey = (0.27, 0.3, 0.32);
};

let windowSize = 800;

let windowSizef = float_of_int windowSize;

let toWorldCoord (WCoord {x, y}) => (
  x /. (windowSizef /. 2.) -. 1.,
  y /. (windowSizef /. 2.) -. 1.
);

let drawRect width::width height::height color::color position::(GCoord {x, y}) => {
  GlDraw.begins `quads;
  GlDraw.color color;
  let bottomLeft = WCoord {x: float_of_int x, y: float_of_int y};
  let topLeft = WCoord {x: float_of_int x, y: float_of_int @@ y + height};
  let topRight = WCoord {x: float_of_int @@ x + width, y: float_of_int @@ y + height};
  let bottomRight = WCoord {x: float_of_int @@ x + width, y: float_of_int y};
  List.iter GlDraw.vertex2 (List.map toWorldCoord [bottomLeft, topLeft, topRight, bottomRight]);
  GlDraw.ends ();
};

let drawPoint gameState::gameState ({x, y, size}) => {
  drawRect
    width::size
    height::size
    color::Color.yellow
    position::(GCoord {x, y});
};

let draw gameState::gameState x::x y::y => {
  let mousePos = {x, y: windowSize - y};
  gameState.drawables = [
    {x: mousePos.x, y: mousePos.y, size: gameState.brushSize},
    ...gameState.drawables
  ];
};

let setBrushSize gameState::gameState amount::amount => {
  let newBrushSize = gameState.brushSize + amount;
  switch newBrushSize {
    | 100 => gameState.brushSize = 100;
    | 10 => gameState.brushSize = 10;
    | _ => gameState.brushSize = newBrushSize;
  }
};

let keyboard gameState::gameState key::key x::x y::y => {
  switch key {
    | 27 => (gameState.drawables = []);
    | 113 => setBrushSize gameState::gameState amount::(-5);
    | 119 => setBrushSize gameState::gameState amount::(5);
    | _ => ();
  }
};

let render gameState::gameState () => {
  GlClear.clear [`color];
  GlMat.load_identity ();
  drawRect
    width::(windowSize)
    height::(windowSize)
    color::Color.black
    position::(GCoord {x: 0, y: 0});
  List.iter (drawPoint gameState::gameState) gameState.drawables;
  Glut.swapBuffers ()
};

let () = {
  ignore @@ Glut.init Sys.argv;
  Glut.initWindowSize windowSize windowSize;
  Glut.initDisplayMode double_buffer::true ();
  ignore @@ Glut.createWindow title::"Reason Draw";
  let gameState = {drawables: [], brushSize: 20};
  GlMat.mode `modelview;
  Glut.motionFunc (draw gameState::gameState);
  Glut.keyboardFunc (keyboard gameState::gameState);
  Glut.displayFunc (render gameState::gameState);
  Glut.idleFunc cb::(Some Glut.postRedisplay);
  Glut.mainLoop ();
};

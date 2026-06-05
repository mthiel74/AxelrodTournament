(* ::Package:: *)

(* ===================================================================== *)
(* post_helpers.wl  --  Cell-construction helpers for building the        *)
(* Wolfram Community notebook programmatically (mirrors the ENSO project).*)
(* Load with Get[...]; then assemble cells with writeAll[{...}].          *)
(* ===================================================================== *)

BeginPackage["PostHelpers`"];

ClearAll["PostHelpers`*", "PostHelpers`Private`*"];

setImageDir::usage = "setImageDir[dir] sets the directory figures are imported from.";
$allCells::usage   = "accumulated list of cells.";
writeAll::usage    = "writeAll[cells] appends cells to $allCells.";
resetCells::usage  = "resetCells[] empties $allCells.";

title::usage="";    subtitle::usage="";  subsubtitle::usage="";
hd1::usage="";       hd2::usage="";       hd3::usage="";
para::usage="";      abstractCell::usage="";  captionCell::usage="";
bold::usage="";      ital::usage="";      mono::usage="";  math::usage="";
displayMath::usage=""; imgCell::usage=""; wlIn::usage=""; codeIn::usage="";
figWithCode::usage="figWithCode[call,file,width] = Input cell with the call + the static image.";
animCell::usage="animCell[file,width] = looping AnimatedImage from a GIF.";
animWithCode::usage="animWithCode[call,file,width] = Input cell with the call + the looping GIF.";
bullets::usage="";   link::usage="";

Begin["`Private`"];

$imgDir = ".";
setImageDir[dir_] := ($imgDir = dir);

$allCells = {};
resetCells[] := ($allCells = {});
writeAll[cs_List] := ($allCells = Join[$allCells, cs];
  Print["  writeAll: +", Length[cs], " cells (total = ", Length[$allCells], ")"];);

(* headings *)
title[t_]       := Cell[t, "Title"];
subtitle[t_]    := Cell[t, "Subtitle"];
subsubtitle[t_] := Cell[t, "Subsubtitle", FontSlant -> Italic, FontColor -> GrayLevel[0.4]];
hd1[t_]         := Cell[t, "Section"];
hd2[t_]         := Cell[t, "Subsection"];
hd3[t_]         := Cell[t, "Subsubsection"];

(* inline styles *)
bold[s_] := StyleBox[s, FontWeight -> Bold];
ital[s_] := StyleBox[s, FontSlant -> Italic];
mono[s_] := StyleBox[s, FontFamily -> "Courier"];
math[s_] := StyleBox[s, FontFamily -> "Times", FontSlant -> Italic];

(* paragraph text: para["..."] or para[{TextData parts}] *)
para[s_String]    := Cell[s, "Text", CellMargins -> {{50, 50}, {6, 6}}];
para[parts_List]  := Cell[TextData[parts], "Text", CellMargins -> {{50, 50}, {6, 6}}];

abstractCell[parts_] := Cell[TextData[Flatten[{parts}]], "Text",
  FontSize -> 14, CellMargins -> {{60, 60}, {6, 6}},
  Background -> RGBColor[0.98, 0.96, 0.90]];

captionCell[s_] := Cell[If[Head[s] === String, s, TextData[Flatten[{s}]]],
  "Text", FontSlant -> Italic, FontSize -> 11, FontColor -> GrayLevel[0.35],
  CellMargins -> {{70, 70}, {2, 14}}];

(* display math from a box expression *)
displayMath[boxes_] := Cell[BoxData @ FormBox[boxes, TraditionalForm],
  "DisplayFormula", CellMargins -> {{80, 50}, {8, 8}}];

(* bullet list from a list of strings/TextData *)
bullets[items_List] := Sequence @@ (
  Cell[If[Head[#] === String, #, TextData[Flatten[{#}]]], "Item"] & /@ items);

(* hyperlink that renders in both PDF and web *)
link[txt_String, url_String] := Sequence[
  StyleBox[txt, FontWeight -> "SemiBold"], " (",
  StyleBox[url, FontFamily -> "Courier", FontSize -> 10], ")"];

(* static image cell from a PNG in $imgDir (legacy; superseded by the
   inline-evaluated figWithCode below, kept for direct callers). *)
imgCell[file_, width_: 680] := Module[{path = FileNameJoin[{$imgDir, file}], im},
  If[FileExistsQ[path],
    im = Image[Import[path], ImageSize -> width];
    Cell[BoxData @ ToBoxes[im], "Output", ShowCellLabel -> False,
      TextAlignment -> Center, CellMargins -> {{Automatic, Automatic}, {10, 6}}],
    Cell[TextData[{bold["[ missing figure: " <> file <> " ]"]}], "Text",
      FontColor -> RGBColor[0.7, 0.1, 0.1], TextAlignment -> Center]]];

(* runnable Wolfram code cell *)
wlIn[code_String] := Cell[code, "Input",
  CellMargins -> {{60, 30}, {6, 6}}, FontSize -> 11];

(* Evaluate `call` at build time and embed the resulting expression as a
   vector Graphics output (no PNG round-trip). Smaller and sharper than
   importing the rasterised PNG; the data files used by the call do NOT
   need to be present when the saved notebook is opened.

   Detects the "PostFigures package not loaded" case (where ToExpression
   returns an unevaluated symbol whose Head lives in the PostFigures
   context) and shows a red placeholder instead of embedding the literal
   source code as if it were the figure.                                *)
evalFigCell[call_String, width_Integer] := Module[{g, sized, didEval},
  (* UsingFrontEnd (outside Check) so figures that need a front end \[Dash]
     MatrixPlot, continuous BarLegend, etc. \[Dash] evaluate instead of
     tripping FrontEndObject::notavail and embedding a placeholder. *)
  g = Quiet @ UsingFrontEnd @ Check[ToExpression[call], $Failed];
  didEval = g =!= $Failed && FreeQ[g, _Symbol?(Context[#] === "PostFigures`" &)];
  If[! didEval,
    Cell[TextData[{bold["[ failed to evaluate: " <> call <>
      " \[Dash] is PostFigures` loaded? ]"]}], "Text",
      FontColor -> RGBColor[0.7, 0.1, 0.1], TextAlignment -> Center],
    sized = Quiet @ Check[Show[g, ImageSize -> width], g];
    Cell[BoxData @ ToBoxes[sized], "Output", ShowCellLabel -> False,
      TextAlignment -> Center, CellMargins -> {{Automatic, Automatic}, {10, 6}}]]];

(* figure with its generating code: a runnable Input cell showing the exact
   call, followed by the call's evaluated graphics (embedded as vector boxes,
   not as a rasterised PNG). The legacy 3-arg signature ignores the PNG
   filename so existing build scripts keep working unchanged. *)
figWithCode[call_String, width_Integer: 680] :=
  Sequence[wlIn[call], evalFigCell[call, width]];
figWithCode[call_String, _String, width_Integer] :=
  Sequence[wlIn[call], evalFigCell[call, width]];

(* Animated GIF embedded as a single AnimatedImage. Critically: do NOT
   ImageResize each frame (that re-encodes every frame as an uncompressed
   PNG inside BoxData, multiplying notebook size by 20-25x). Instead let
   Import return the native frames and use ImageSize on the AnimatedImage. *)
animCell[file_, width_: 680] := Module[{path = FileNameJoin[{$imgDir, file}]},
  If[FileExistsQ[path],
    Cell[BoxData @ ToBoxes @ AnimatedImage[Import[path, "GIF"],
        ImageSize -> width, AnimationRepetitions -> Infinity],
      "Output", ShowCellLabel -> False, TextAlignment -> Center,
      CellMargins -> {{Automatic, Automatic}, {10, 6}}],
    Cell[TextData[{bold["[ missing animation: " <> file <> " ]"]}], "Text",
      FontColor -> RGBColor[0.7, 0.1, 0.1], TextAlignment -> Center]]];

(* animation with its generating code (call shown, then the looping GIF) *)
animWithCode[call_String, file_, width_: 680] := Sequence[wlIn[call], animCell[file, width]];

(* plain code/shell cell (no WL syntax highlighting) *)
codeIn[code_String] := Cell[code, "Program",
  CellMargins -> {{60, 30}, {6, 6}}, FontFamily -> "Courier", FontSize -> 11,
  ShowStringCharacters -> False, Background -> RGBColor[0.95, 0.96, 0.98]];

End[];
EndPackage[];

Print["PostHelpers loaded."];

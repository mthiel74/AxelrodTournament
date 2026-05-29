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

(* static image cell from a PNG in $imgDir *)
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

(* plain code/shell cell (no WL syntax highlighting) *)
codeIn[code_String] := Cell[code, "Program",
  CellMargins -> {{60, 30}, {6, 6}}, FontFamily -> "Courier", FontSize -> 11,
  ShowStringCharacters -> False, Background -> RGBColor[0.95, 0.96, 0.98]];

End[];
EndPackage[];

Print["PostHelpers loaded."];

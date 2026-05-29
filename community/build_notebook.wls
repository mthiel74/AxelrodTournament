#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ===================================================================== *)
(* build_notebook.wls -- assemble the Wolfram Community post notebook     *)
(* (community/axelrod_tournament.nb + .pdf) from Cell[] expressions.      *)
(*                                                                        *)
(* Mirrors the ENSO-emergence pipeline: cells are built with the helpers  *)
(* in post_helpers.wl, accumulated via writeAll[], then persisted with    *)
(* UsingFrontEnd[CreateDocument[...]; NotebookSave[...]].                  *)
(*                                                                        *)
(* Figures are read from docs/images (run wolfram/figures.wls first).     *)
(* ===================================================================== *)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get[FileNameJoin[{repoRoot, "community", "post_helpers.wl"}]];
Get[FileNameJoin[{repoRoot, "wolfram", "axelrod.wl"}]];

imgDir = FileNameJoin[{repoRoot, "docs", "images"}];
outNb  = FileNameJoin[{repoRoot, "community", "axelrod_tournament.nb"}];
outPdf = FileNameJoin[{repoRoot, "community", "axelrod_tournament.pdf"}];

PostHelpers`setImageDir[imgDir];
PostHelpers`resetCells[];

(* bring helper names into scope unqualified (alias each symbol) *)
title       = PostHelpers`title;       subtitle    = PostHelpers`subtitle;
subsubtitle = PostHelpers`subsubtitle; hd1         = PostHelpers`hd1;
hd2         = PostHelpers`hd2;         hd3         = PostHelpers`hd3;
para        = PostHelpers`para;        abstractCell= PostHelpers`abstractCell;
captionCell = PostHelpers`captionCell; bold        = PostHelpers`bold;
ital        = PostHelpers`ital;        mono        = PostHelpers`mono;
math        = PostHelpers`math;        displayMath = PostHelpers`displayMath;
imgCell     = PostHelpers`imgCell;     wlIn        = PostHelpers`wlIn;
codeIn      = PostHelpers`codeIn;      bullets     = PostHelpers`bullets;
link        = PostHelpers`link;        writeAll    = PostHelpers`writeAll;

(* helper: pull a few rows from a data CSV to show live numbers *)
dataDir = FileNameJoin[{repoRoot, "data"}];
csv[f_] := Import[FileNameJoin[{dataDir, f}], "CSV"];

(* ===================================================================== *)
(* TITLE + ABSTRACT                                                       *)
(* ===================================================================== *)
writeAll[{
  title["Re-running Axelrod's Tournament: Cooperation, Evolution, and Noise"],
  subtitle["The iterated Prisoner's Dilemma in the Wolfram Language \[Dash] \
from Tit-for-Tat to an evolutionarily stable mix that depends on noise"],
  subsubtitle["Wolfram Community submission \[Dash] May 2026"],

  imgCell["replicator_vs_noise.png", 720],
  captionCell["The headline result. As execution noise \[Epsilon] rises \
(left to right), the evolutionarily stable composition of strategies shifts \
\[Dash] from a coexistence of generous reciprocators to a collapse into \
unconditional defection."],

  hd1["Abstract"],
  abstractCell[{
    "In 1980 Robert Axelrod ran a now-famous computer tournament for the ",
    bold["iterated Prisoner's Dilemma"],
    ". The winner was the simplest entrant of all \[Dash] ",
    bold["Tit-for-Tat"],
    ": cooperate first, then copy your opponent. This notebook reproduces \
that tournament in the Wolfram Language, with the classic strategies plus \
modern entrants (Generous Tit-for-Tat, Pavlov, Gradual, and Press \[Dash] \
Dyson zero-determinant strategies)."}],
  abstractCell[{
    "It then does two things Axelrod's setup hints at but does not show. ",
    bold["First"], ", an ", ital["evolutionary"],
    " tournament: we let populations of strategies reproduce in proportion \
to their success and ask which mixture is stable. ", bold["Second"],
    ", we add ", ital["noise"],
    " \[Dash] a small probability that an intended move is mis-executed \
\[Dash] and show that the evolutionarily stable mixture ",
    bold["depends on the noise level"],
    ". Finally, in the spirit of an automated-research loop, we let a search \
algorithm ", ital["discover"],
    " memory-one strategies that beat Tit-for-Tat under noise."}]
}];

(* ===================================================================== *)
(* 1. THE GAME                                                            *)
(* ===================================================================== *)
writeAll[{
  hd1["1. The Prisoner's Dilemma, and why iteration changes everything"],
  para[{"Two players each choose, simultaneously, to ", bold["cooperate"],
    " (C) or ", bold["defect"], " (D). The payoffs satisfy ",
    math["T > R > P > S"], " and ", math["2R > T + S"],
    ". With the classic Axelrod values ",
    math["T=5, R=3, P=1, S=0"], ":"}],
  displayMath[GridBox[{
     {"", StyleBox["opp C", FontSlant -> Italic], StyleBox["opp D", FontSlant -> Italic]},
     {StyleBox["I C", FontSlant -> Italic], "R=3", "S=0"},
     {StyleBox["I D", FontSlant -> Italic], "T=5", "P=1"}},
     RowLines -> True, ColumnLines -> True]],
  para[{"In a ", ital["single"],
    " game, defection strictly dominates: whatever the opponent does, you \
score more by defecting. Yet mutual cooperation (R=3 each) beats mutual \
defection (P=1 each). That is the dilemma. Axelrod's insight was that when \
the game is ", bold["repeated"],
    ", the shadow of the future can sustain cooperation \[Dash] a strategy \
can reward cooperation and punish defection in later rounds."}],

  hd2["Encoding strategies in the Wolfram Language"],
  para[{"We encode ", mono["C = 1"], " and ", mono["D = 0"],
    ". A strategy is a pure function of the two histories of actual moves; \
the smallest interesting family is the ", bold["memory-one"],
    " strategies, parameterised by the probability of cooperating after each \
of the four possible previous outcomes:"}],
  wlIn["memOne[open_, {pCC_, pCD_, pDC_, pDD_}] := Function[{me, opp},
  If[me === {},
    If[RandomReal[] < open, 1, 0],
    If[RandomReal[] < {pCC, pCD, pDC, pDD}[[ stateIndex[Last[me], Last[opp]] ]], 1, 0]]];

(* Tit-for-Tat: cooperate first, then copy the opponent's last move *)
TitForTat = memOne[1, {1, 0, 1, 0}];
Pavlov    = memOne[1, {1, 0, 0, 1}];   (* win-stay, lose-shift *)
GenerousTFT = memOne[1, {1, 1/3, 1, 1/3}];"],
  para[{"The full library (in ", mono["wolfram/axelrod.wl"],
    ") has 13 strategies: AllC, AllD, Random, Tit-for-Tat, Suspicious TFT, \
Generous TFT, Tit-for-2-Tats, 2-Tits-for-Tat, Grim, Pavlov, Gradual, and two \
zero-determinant strategies (an extortioner and a generous one)."}],

  hd2["Noise: the trembling hand"],
  para[{"Real interactions are imperfect. We model ", bold["execution noise"],
    ": each intended move is flipped with probability ", math["\[Epsilon]"],
    ", and the opponent sees the ", ital["actual"],
    " (post-noise) move. This single knob turns out to control everything \
that follows."}]
}];

(* ===================================================================== *)
(* 2. THE CLASSIC TOURNAMENT                                              *)
(* ===================================================================== *)
Module[{r0 = csv["ranking_noise0..csv"]},
writeAll[{
  hd1["2. The classic round-robin (reproducing Axelrod 1980)"],
  para[{"Every strategy plays every strategy (including itself) for 200 \
rounds, repeated 100 times to average out randomness. Each strategy's score \
is its mean per-round payoff against the whole field. At ",
    math["\[Epsilon] = 0"], " (no noise) the ranking is:"}],
  imgCell["ranking_grid.png", 720],
  captionCell["Field score of each strategy at four noise levels. At \
\[Epsilon]=0 the generous reciprocators lead; the extortioner (ZD-Extort-2) \
finishes last."],
  para[{bold["The classic lesson holds."],
    " Nice, reciprocal, forgiving strategies dominate. Notably, plain \
Tit-for-Tat is ", ital["edged out"],
    " by its more forgiving cousins \[Dash] Generous TFT and Tit-for-2-Tats \
\[Dash] exactly the refinement Axelrod found in his second tournament. And \
the zero-determinant ", bold["extortioner finishes dead last"],
    ": extortion wins individual matches but poisons the cooperative \
environment it depends on."}],
  para[{"The payoff structure of who-beats-whom:"}],
  imgCell["payoff_heatmap.png", 560],
  captionCell["Mean per-round payoff, row strategy against column strategy \
(\[Epsilon]=0). Warm = high. The cooperative cluster (top-left block) sustains \
mutual R=3; AllD and the extortioner do well in single matchups but drag the \
whole field down."],

  hd2["What noise does to the ranking"],
  imgCell["score_vs_noise.png", 720],
  captionCell["As noise rises, every strategy's score is pulled toward the \
mutual-defection payoff P=1, and the gap between 'nice' and 'nasty' \
strategies narrows. Unforgiving strategies (Grim, Gradual) fall fastest: a \
single accidental defection traps them in retaliation."]
}]];

(* ===================================================================== *)
(* 3. THE EVOLUTIONARY TOURNAMENT                                         *)
(* ===================================================================== *)
writeAll[{
  hd1["3. The evolutionary tournament \[Dash] the stable mix depends on noise"],
  para[{"Axelrod's deeper question was evolutionary: if successful strategies \
reproduce, which ones survive? We answer it two ways."}],

  hd2["Replicator dynamics"],
  para[{"Let ", math["x"], " be the vector of population fractions and ",
    math["A"], " the mean-payoff matrix (",
    math["A[i,j]"], " = score of ", math["i"], " against ", math["j"],
    "). Each strategy grows in proportion to its fitness relative to the \
population average:"}],
  displayMath[RowBox[{
    SubscriptBox["x", "i"], "'", "=", SubscriptBox["x", "i"],
    RowBox[{"(", RowBox[{
      SubscriptBox[RowBox[{"(", RowBox[{"A", ".", "x"}], ")"}], "i"],
      "-", RowBox[{"x", ".", "A", ".", "x"}]}], ")"}]}]],
  wlIn["replicator[A_, steps_, mut_] := Module[{x = ConstantArray[1./n, n], f, phi},
  Do[ f = A . x; phi = x . f;
      x = x f / phi;              (* selection           *)
      x = (1 - mut) x + mut/n;    (* small mutation term  *)
      x = x / Total[x],
    {steps}];
  x]"],
  para[{"Sweeping the noise level and recording the stable composition gives \
the headline figure (top of this post). It reveals ", bold["three regimes"], ":"}],
  Cell[TextData[{bold["Low noise (\[Epsilon] \[TildeTilde] 0): "],
    "a coexistence of cooperative strategies \[Dash] Generous TFT, \
Tit-for-2-Tats, ZD-Generous, Tit-for-Tat and Pavlov share the population. \
Cooperation is evolutionarily robust."}], "Item"],
  Cell[TextData[{bold["Intermediate noise: "],
    "the forgiving reciprocators (Generous TFT, Tit-for-2-Tats, ZD-Generous) \
dominate and trade the lead among themselves \[Dash] error-correction is at \
a premium."}], "Item"],
  Cell[TextData[{bold["High noise (\[Epsilon] \[GreaterEqual] 0.2): "],
    "cooperation collapses. AllD sweeps to ~99% of the population: under \
heavy noise, reciprocators spend so long accidentally punishing each other \
that unconditional defection becomes the only stable attractor."}], "Item"],

  hd2["A finite-population check: the Moran process"],
  para[{"Replicator dynamics is a deterministic, infinite-population limit. \
To confirm the result is not an artefact of that idealisation, we also run a \
stochastic ", bold["Moran process"],
    " (population N=100, frequency-dependent birth-death with rare mutation) \
and record the long-run average composition."}],
  imgCell["moran_vs_noise.png", 720],
  captionCell["Moran process, long-run composition vs noise. Finite-population \
stochasticity makes the picture grainier, but the qualitative story survives: \
forgiving cooperators at low-to-moderate noise, defection-dominated outcomes \
as noise grows."],
  para[{bold["Takeaway. "],
    "\"Which strategy is evolutionarily stable?\" has ",
    ital["no noise-independent answer"],
    ". The same population that converges on generous reciprocity in a clean \
channel converges on pure defection in a noisy one."}]
}];

(* ===================================================================== *)
(* 4. STRATEGY DISCOVERY                                                  *)
(* ===================================================================== *)
Module[{ds = csv["discovered_strategies.csv"]},
writeAll[{
  hd1["4. Discovering strategies that beat Tit-for-Tat under noise"],
  para[{"Can we do better than the textbook strategies? In the spirit of an \
automated-research loop \[Dash] ", ital["propose \[Rule] evaluate \[Rule] \
accept \[Rule] repeat"],
    " \[Dash] we search the memory-one space ",
    math["\[Theta] = (open, pCC, pCD, pDC, pDD) \[Element] [0,1]^5"],
    " for the best response to the standard field at each noise level. A \
greedy hill-climb gives the narrative; a small genetic algorithm is the \
workhorse."}],
  wlIn["fitness[theta_, eps_] := Mean[
   meanMatch[memOne[theta[[1]], theta[[2;;]]], #, rounds, eps, reps][[1]] & /@ field];

(* greedy accept/reject loop -- the autoresearch analogy *)
champion = {1, 1, 0, 1, 0};                       (* start from Tit-for-Tat *)
Do[ proposal = clip[champion + RandomVariate[NormalDistribution[0, .12], 5]];
    If[fitness[proposal, eps] > fitness[champion, eps], champion = proposal],
  {150}]"],
  imgCell["discovery_progress.png", 640],
  captionCell["Greedy discovery: best field score found so far, starting from \
Tit-for-Tat, at two noise levels. Each upward step is an accepted proposal."],

  hd2["The discovered strategies beat Tit-for-Tat at every noise level"],
  imgCell["discovered_vs_tft.png", 680],
  captionCell["Discovered best-response strategy (solid) vs plain Tit-for-Tat \
(dashed) in field score, across noise. The discovered strategy wins \
everywhere; the margin is largest where noise matters most."],
  para[{"And the ", ital["shape"], " of the optimal strategy drifts with noise:"}],
  imgCell["discovered_params_vs_noise.png", 680],
  captionCell["The discovered memory-one probabilities vs noise. The \
forgiveness parameter pCD (cooperate again after being suckered) rises with \
noise \[Dash] error-correction becomes essential \[Dash] until, at very high \
noise, the best response against a decaying field tilts back toward defection."],
  para[{"This is the same lesson as the evolutionary tournament, reached by a \
different route: ", bold["the optimal behaviour is a function of the \
environment"], ", and noise is the knob that turns generous reciprocity into \
defection."}]
}]];

(* ===================================================================== *)
(* 5. CONCLUSION + REPRODUCIBILITY                                        *)
(* ===================================================================== *)
writeAll[{
  hd1["5. Conclusions"],
  bullets[{
    {bold["Tit-for-Tat is excellent but not optimal. "],
     "In a cooperative field, more forgiving reciprocators (Generous TFT, \
Tit-for-2-Tats) edge it out \[Dash] and extortion finishes last."},
    {bold["Evolutionary stability is noise-dependent. "],
     "Replicator and Moran dynamics agree: generous cooperation is stable at \
low noise; unconditional defection takes over at high noise."},
    {bold["Forgiveness is the answer to noise. "],
     "Both the evolutionary winners and the algorithmically discovered \
strategies become more forgiving as noise rises \[Dash] up to the point where \
the whole field collapses."}
  }],
  hd1["Reproducing this work"],
  para[{"Everything here is a few seconds to a few minutes of Wolfram \
Language \[Dash] no external data, no API keys. The full source is on GitHub."}],
  codeIn["wolframscript -file tests/sanity.wls        # correctness checks
wolframscript -file wolfram/tournament.wls  # round-robin ranking
wolframscript -file wolfram/evolution.wls   # replicator + Moran vs noise
wolframscript -file wolfram/discover.wls     # strategy discovery
wolframscript -file wolfram/figures.wls      # regenerate all figures"],
  para[{ital["Built entirely in the Wolfram Language. The strategy-discovery \
loop borrows the propose \[Rule] evaluate \[Rule] accept shape of \
automated-research scaffolds, re-implemented natively in WL."]}]
}];

(* ===================================================================== *)
(* ASSEMBLE + EXPORT                                                      *)
(* ===================================================================== *)
allCells = PostHelpers`$allCells;
Print["[notebook] assembled ", Length[allCells], " cells"];

TimeConstrained[
  UsingFrontEnd[
    nbObj = CreateDocument[allCells, Visible -> False,
      WindowSize -> {1000, 800}, StyleDefinitions -> "Default.nb"];
    NotebookSave[nbObj, outNb];
    Print["[notebook] wrote ", outNb, " (", FileByteCount[outNb], " B)"];
    Quiet @ Check[
      Export[outPdf, nbObj];
      Print["[notebook] wrote ", outPdf, " (", FileByteCount[outPdf], " B)"],
      Print["[notebook] warn: PDF export failed"]];
    NotebookClose[nbObj];
  ],
  600,
  Print["[notebook] warn: notebook save timed out"]
];
Print["[notebook] done."];

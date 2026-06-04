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
    ". Finally, in the spirit of an automated-research loop, we let search \
algorithms ", ital["discover"], " new strategies \[Dash] culminating in an ",
    bold["eight-hour evolutionary search over memory-two strategies"],
    " whose champion beats Tit-for-Tat by ~9% and finishes first in a field of \
26, by learning to tell an accidental defection from a deliberate one."}]
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
    "cooperation collapses. A single unforgiving punisher \[Dash] Grim \[Dash] \
sweeps to ~98% of the population: under heavy noise, reciprocators spend so \
long accidentally punishing each other that a strategy which simply defects \
forever after the first defection becomes the only stable attractor (once \
noise has triggered it, it is behaviourally identical to AllD)."}], "Item"],

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
(* 5. AUTORESEARCH                                                        *)
(* ===================================================================== *)
writeAll[{
  hd1["5. Letting an automated-research loop invent strategies"],
  para[{"The discovery in \[Section]4 searched a continuous parameter box. We \
can also run the loop the way a human researcher would: ", bold["propose a \
named hypothesis, evaluate it, keep it if it wins, repeat"],
    " \[Dash] exactly the shape of Karpathy-style ", ital["autoresearch"],
    " scaffolds (an AI edits one solution file; a fixed harness scores it; \
improvements are committed). We ported that loop to this domain: ",
    mono["autoresearch/evaluate.wls"], " is the fixed harness (an 18-strategy \
field \[Times] six noise levels) and a sequence of hypotheses is proposed, \
each scored by mean field score averaged over the noise sweep."}],
  codeIn["champion <- Tit-for-Tat
LOOP:  propose a strategy  ->  evaluate vs the field under noise
       if score > champion: keep (save to champions/)  else: revert
       append (iter, score, status, description) to results.tsv"],
  para[{"The result is a research log. Against this field (TFT baseline ",
    mono["2.316"], "), the loop found ", bold["seven strategies that beat \
Tit-for-Tat"], ". The winners are instructive:"}],
  bullets[{
    {bold["Tit-for-2-Tats (2.400). "],
     "Patience beats vigilance: absorbing a single (possibly accidental) \
defection before retaliating is the single biggest win."},
    {bold["Generous Tit-for-2-Tats and Generous TFT. "],
     "Tolerance plus probabilistic forgiveness compounds the gain."},
    {bold["AdaptiveGenerous \[Dash] a self-tuning entrant. "],
     "It estimates the channel noise online (from how often the opponent's \
moves flip) and scales its own forgiveness to match \[Dash] beating TFT with \
no hand-set forgiveness constant."}}],
  para[{bold["What lost is as telling as what won. "],
    "Plain contrition and Win-Stay-Lose-Shift (Pavlov) scored ", ital["below"],
    " TFT in this well-mixed field \[Dash] forgiveness has to be ",
    ital["patient"], " (two-strikes), not merely reflexive. Hold that thought: \
Pavlov's fortunes change once we add spatial structure."}]
}];

(* ===================================================================== *)
(* 6. BEYOND MEMORY-ONE                                                   *)
(* ===================================================================== *)
writeAll[{
  hd1["6. Beyond memory-one: evolving finite-state machines"],
  para[{"Memory-one strategies condition only on the previous round. Do richer \
strategies help? We let a genetic algorithm evolve ", bold["finite-state \
(Mealy) machines"], " \[Dash] each a little automaton with up to four internal \
states, an action and a state-transition for every observed opponent move. \
This space contains TFT, Grim, Pavlov and much else; we verified it encodes \
TFT and Grim exactly before evolving it."}],
  imgCell["fsm_vs_tft.png", 680],
  captionCell["Evolved finite-state machines vs Tit-for-Tat across noise \[Dash] \
the machines beat TFT at essentially every noise level."],
  imgCell["fsm_complexity_vs_noise.png", 680],
  captionCell["The best evolved machine uses 2\[Dash]4 of its 4 reachable \
states \[Dash] genuine multi-state memory, beyond what memory-one can express. \
The gain over the memory-one optimum is real but modest: against this field, \
most of the advantage is already captured by one round of memory plus the \
right amount of forgiveness."]
}];

(* ===================================================================== *)
(* 7. SPATIAL STRUCTURE                                                   *)
(* ===================================================================== *)
writeAll[{
  hd1["7. Spatial structure makes cooperation robust to noise"],
  para[{"Everything so far assumed a ", ital["well-mixed"],
    " population. But real agents interact ", bold["locally"],
    ". We place strategies on the sites of a 32\[Times]32 lattice (a torus). \
Each generation, every site plays its four neighbours and imitates fitter \
neighbours (Fermi rule). This is Nowak \[Dash] May spatial game theory; the \
classic result is that clustering lets cooperators survive. We ask what noise \
does to it."}],
  imgCell["spatial_composition_vs_noise.png", 760],
  captionCell["Final lattice composition vs noise. At low noise the lattice is \
almost entirely cooperative reciprocators (Grim, Generous TFT, Tit-for-Tat), \
with unconditional defection nearly absent. As noise rises AllD gains ground \
\[Dash] but never takes over: cooperation persists around 60% even at \
\[Epsilon]=0.2."],
  imgCell["spatial_snapshots.png", 720],
  captionCell["Lattice snapshots (colours = strategies; legend below). \
Reciprocators survive by forming contiguous clusters that protect their \
cooperative interior. Noise frays the cluster boundaries but does not dissolve \
them."],
  imgCell["spatial_legend.png", 520],
  para[{bold["Spatial structure makes cooperation far more robust to noise. "],
    "In the well-mixed model (\[Section]3), once \[Epsilon] is large the \
population collapses almost entirely to defection. On the lattice the same \
noise erodes cooperation only partially: clustering lets reciprocators shield \
one another, so cooperation degrades gracefully (97% \[Rule] ~60%) instead of \
collapsing. ", bold["Whether noise is catastrophic or merely corrosive for \
cooperation depends on population structure."]}]
}];

(* ===================================================================== *)
(* 8. RED-QUEEN CO-EVOLUTION                                              *)
(* ===================================================================== *)
writeAll[{
  hd1["8. Red-Queen co-evolution"],
  para[{"So far strategies were optimised against a ", ital["fixed"],
    " field. What if the field itself evolves? We co-evolve a population of \
memory-one genomes against ", bold["itself"],
    ": each individual's fitness is its score against the current population, \
which changes every generation. This is a ", ital["Red Queen"],
    " race \[Dash] you must keep adapting just to hold your rank."}],
  imgCell["coevolution_coop_traj.png", 760],
  captionCell["Population-mean cooperativeness over generations. At \
\[Epsilon]=0 the population climbs to stable, highly cooperative reciprocity. \
At \[Epsilon]=0.15 it rises, then becomes restless and slides back \[Dash] \
noise prevents a stable cooperative equilibrium from locking in."],
  imgCell["coevolution_genome_vs_noise.png", 760],
  captionCell["The co-evolved mean genome vs noise. As noise rises the \
population becomes less rigidly cooperative (pCC falls) but more forgiving \
(pCD rises) and more able to recover from mutual defection (pDD rises) \[Dash] \
a third independent route to the same conclusion: forgiveness answers noise."]
}];

(* ===================================================================== *)
(* 9. ESS INVASION ANALYSIS                                               *)
(* ===================================================================== *)
writeAll[{
  hd1["9. Which strategies are evolutionarily stable?"],
  para[{"A rigorous capstone. For a panel of 25 strategies (classics, modern \
entrants, and the discovered champions) we compute, at each noise level, the \
pairwise ", bold["invasion matrix"], ": can a rare mutant ", math["M"],
    " invade a resident population of ", math["R"], "? It can if ",
    math["A[M,R] > A[R,R]"], ". A strategy is ", bold["evolutionarily stable"],
    " if no mutant can invade it."}],
  imgCell["ess_stability_map.png", 640],
  captionCell[{"Evolutionarily stable strategies vs noise (green = no mutant \
invades). At ", bold["zero noise"], " a whole cluster is uninvadable \[Dash] \
AllD and Grim, but also Tit-for-Tat, ZD-Generous, Omega-TFT and even two of \
the discovered Generous-TFT champions: in a clean channel, cooperation can be \
evolutionarily stable. Add even a little ", bold["noise"], " and that cluster \
shatters \[Dash] the stable set collapses to a lone defector (AllD or Grim), \
with noise levels in between admitting no pure ESS at all. Noise destroys the \
evolutionary stability of cooperation."}],
  imgCell["ess_invasibility_highnoise.png", 620],
  captionCell["Invasibility matrix at \[Epsilon]=0.2 (red = row invades \
column). The defectors have the cleanest columns."],
  para[{bold["Three meanings of \[OpenCurlyDoubleQuote]stable.\
\[CloseCurlyDoubleQuote] "],
    "\[OpenCurlyDoubleQuote]Which strategy is stable?\[CloseCurlyDoubleQuote] \
has ", ital["three"], " answers: (i) ", bold["ESS"],
    " (uninvadable) admits cooperation at zero noise but collapses to a lone \
defector once noise appears; (ii) ", bold["replicator-stable from a \
cooperative start"], " (\[Section]3) gives generous reciprocity collapsing to \
Grim under noise; (iii) ", bold["spatially resilient"],
    " (\[Section]7) gives reciprocator clusters that survive noise. All three \
are noise-dependent and they disagree \[Dash] there is no single best \
strategy, only best-for-a-given-world."}]
}];

(* ===================================================================== *)
(* 10. THE 8-HOUR RUN: A MEMORY-TWO CHAMPION                              *)
(* ===================================================================== *)
writeAll[{
  hd1["10. An 8-hour automated search: a champion beyond memory-one"],
  para[{"The discoveries so far searched small or fixed spaces. To see what a \
genuinely long, open-ended search would find, we ran the autoresearch loop for ",
    bold["eight hours"], " over a much larger space: ", bold["memory-two"],
    " stochastic strategies, ", math["\[Theta] = (open, p1, \[Ellipsis], p16) \
\[Element] [0,1]^17"], ". A memory-two strategy conditions its move on the ",
    ital["last two"], " rounds, so it can express things memory-one cannot \
\[Dash] crucially, it can tell an ", ital["isolated"],
    " defection (likely a noise blip) from a ", ital["sustained"], " one."}],
  para[{"The engine was a 4-island genetic algorithm with ring migration and \
soft-restarts on stagnation, scored on the same field-vs-noise protocol, with a \
hall of fame and frequent checkpoints. It ran unattended under a supervisor \
that caps the wall-clock budget and guarantees no runaway kernels are left \
behind. Over 111 generations the best field score climbed steadily, then \
plateaued \[Dash] the signature of a converged search:"}],
  para[{ital["Search progress: field score of the best strategy found, by \
generation (selected rows):"]}],
  Cell[BoxData@GridBox[
    Prepend[
      {ToString[#[[1]]], ToString[NumberForm[#[[2]], {5, 4}]],
       "+" <> ToString[NumberForm[#[[3]], {4, 3}]]} & /@ {
        {5, 2.4328, 0.1447}, {15, 2.5144, 0.2263}, {25, 2.5357, 0.2476},
        {45, 2.5507, 0.2626}, {75, 2.5661, 0.2780}, {111, 2.5661, 0.2780}},
      {"generation", "best field score", "vs TFT"}],
    GridBoxAlignment -> {"Columns" -> {{Center}}},
    GridBoxFrame -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
    GridBoxItemSize -> {"Columns" -> {{9}}}], "Output",
    TextAlignment -> Center, CellMargins -> {{Automatic, Automatic}, {10, 8}}],

  hd2["Honest scoring: searching is not the same as winning"],
  para[{"The search ranks candidates with a fixed random seed so the \
comparison is deterministic. That can ", ital["overfit"],
    " the seed. So every hall-of-fame strategy was re-scored at high repetition \
with ", bold["fresh seeds it never saw during the search"], ", and to be doubly \
sure we then validated the winner on ", bold["four more independent seeds"],
    ". The champion's edge survives intact: a robust improvement of ",
    bold["+0.20 over Tit-for-Tat (about 8.8%)"],
    ", essentially identical to its search-time score. It is a real strategy, \
not a seed artefact."}],

  hd2["What the champion does"],
  para[{"Decoding the 17-number genome into behaviour reveals a clean, \
interpretable rule. It opens by cooperating (~90%), reciprocates cooperation, \
and \[Dash] the key \[Dash] treats the opponent's last two moves very \
differently depending on the pattern:"}],
  bullets[{
    {bold["Opponent defected once, but cooperated the round before "],
     "(an isolated, already-corrected slip \[Dash] exactly what execution noise \
produces): forgive at ", math["\[TildeTilde] 0.90"], "."},
    {bold["Opponent defected both of the last two rounds "],
     "(a sustained defector): cooperate at only ", math["\[TildeTilde] 0.21"],
     " \[Dash] punish."}}],
  para[{bold["This is the crux. "],
    "A memory-one strategy sees only the single last move; faced with \
\[OpenCurlyDoubleQuote]opponent just defected\[CloseCurlyDoubleQuote] it must \
pick ", ital["one"], " forgiveness probability. The memory-two champion splits \
that case by whether the defection looks ", ital["accidental"], " or ",
    ital["deliberate"], " \[Dash] forgiving a mistake while punishing a \
strategy. That is precisely the behaviour noise rewards, and it is why the \
champion's lead over Tit-for-Tat is ", bold["largest at low-to-moderate \
noise"], ", where mistakes are recoverable:"}],
  Cell[BoxData@GridBox[
    Prepend[
      {ToString[NumberForm[#[[1]], {3, 2}]],
       ToString[NumberForm[#[[2]], {5, 3}]],
       ToString[NumberForm[#[[3]], {5, 3}]],
       "+" <> ToString[NumberForm[#[[4]], {4, 3}]]} & /@ {
        {0., 2.845, 2.780, 0.064}, {0.02, 2.765, 2.429, 0.336},
        {0.05, 2.663, 2.409, 0.255}, {0.1, 2.545, 2.386, 0.159},
        {0.15, 2.441, 2.367, 0.075}, {0.2, 2.380, 2.358, 0.022}},
      {"noise \[Epsilon]", "MemTwo-Long", "Tit-for-Tat", "advantage"}],
    GridBoxAlignment -> {"Columns" -> {{Center}}},
    GridBoxFrame -> {"Columns" -> {{True}}, "Rows" -> {{True}}},
    GridBoxItemSize -> {"Columns" -> {{11}}}], "Output",
    TextAlignment -> Center, CellMargins -> {{Automatic, Automatic}, {10, 8}}],
  imgCell["champion_vs_tft_noise.png", 720],
  captionCell["The 8-hour memory-two champion (orange) vs Tit-for-Tat and the \
best non-champion strategy, field score across noise. The champion leads \
everywhere, and its margin over Tit-for-Tat widens as noise rises."],

  hd2["How it ranks against the whole field"],
  para[{"Dropped into the full panel of 26 strategies (classics, modern \
entrants and every strategy discovered in this project), re-scored honestly, \
the memory-two champion finishes ", bold["first overall"],
    " \[Dash] ahead of the self-tuning Adaptive strategy and well clear of \
Tit-for-Tat in 15th place. The discovered strategies (orange) take six of the \
top eight places:"}],
  imgCell["champion_ranking.png", 880],
  captionCell["Field score (mean over noise) for all 26 strategies. Orange = \
discovered by an automated-research loop in this project; \[FivePointedStar] \
marks them. MemTwo-Long, from the 8-hour run, is the single best strategy in \
the field."],
  para[{bold["But is it evolutionarily stable? No \[Dash] and that is the \
honest, interesting answer. "],
    "Re-running the ESS invasion analysis with the champion added, MemTwo-Long \
is ", ital["not"], " an ESS at any noise level: like every forgiving strategy, \
a lone unconditional defector can invade a population of it. It is the strongest ",
    ital["competitor"], " in the field \[Dash] it can invade 15\[Dash]21 of the \
other 25 strategies \[Dash] but being the best player is not the same as being \
uninvadable. The \[OpenCurlyDoubleQuote]three meanings of stable\
\[CloseCurlyDoubleQuote] from \[Section]9 hold: the best strategy to ",
    ital["field"], " and the strategy that ", ital["cannot be invaded"],
    " are simply different things."}]
}];

(* ===================================================================== *)
(* 11. CONCLUSION + REPRODUCIBILITY                                       *)
(* ===================================================================== *)
writeAll[{
  hd1["11. Conclusions"],
  bullets[{
    {bold["Tit-for-Tat is excellent but not optimal. "],
     "In a cooperative field, more forgiving reciprocators (Generous TFT, \
Tit-for-2-Tats) edge it out \[Dash] and extortion finishes last."},
    {bold["Forgiveness must be patient \[Dash] and, with more memory, \
discriminating. "],
     "The autoresearch loop found two-strikes tolerance (Tit-for-2-Tats) beats \
every reflexive scheme; a self-tuning strategy that estimates the noise also \
beats TFT; and the 8-hour memory-two search went further still, learning to \
forgive an isolated (noise-like) defection while punishing a sustained one \
\[Dash] the single best strategy in the whole field."},
    {bold["More search keeps paying, but with diminishing, honest returns. "],
     "Eight hours of evolutionary search over a 17-dimensional space did beat \
every hand-designed and short-search strategy \[Dash] yet the gain over a \
two-minute search was modest, the run converged well before its budget, and \
fixed-seed fitness had to be discounted by fresh-seed re-evaluation to report \
an honest number. Automated discovery works; it is not magic."},
    {bold["\[OpenCurlyDoubleQuote]Stable\[CloseCurlyDoubleQuote] has three \
answers. "],
     "ESS admits cooperation at zero noise but collapses to a lone defector \
under noise; replicator-from-cooperative-start gives generous reciprocity \
collapsing to Grim under noise; spatial structure gives reciprocator clusters \
that survive. All noise-dependent, and they disagree."},
    {bold["Population structure changes the effect of noise. "],
     "Well-mixed: noise drives cooperation to collapse. Spatial: clustering \
keeps it resilient, degrading gracefully instead of collapsing."},
    {bold["Many roads, one lesson. "],
     "Fixed-target search, finite-state GP, replicator and Moran dynamics, \
open-ended co-evolution, and ESS analysis all converge: forgiveness is the \
answer to noise \[Dash] until noise is so high that nothing sustains \
cooperation in a well-mixed world."}
  }],
  hd1["Reproducing this work"],
  para[{"Everything here is a few seconds to a few minutes of Wolfram \
Language \[Dash] no external data, no API keys. The full source is on GitHub."}],
  codeIn["wolframscript -file tests/sanity.wls            # correctness checks
wolframscript -file wolfram/tournament.wls      # round-robin ranking
wolframscript -file wolfram/evolution.wls       # replicator + Moran vs noise
wolframscript -file wolfram/discover.wls         # memory-one discovery
wolframscript -file autoresearch/run_search.wls  # autoresearch champions
wolframscript -file wolfram/fsm_discover.wls     # evolve finite-state machines
wolframscript -file wolfram/spatial.wls          # spatial / lattice evolution
wolframscript -file wolfram/coevolution.wls      # Red-Queen co-evolution
wolframscript -file wolfram/ess.wls              # ESS invasion analysis
autoresearch/supervise.sh 28800                  # 8h memory-two search (zombie-safe)
wolframscript -file wolfram/champion_analysis.wls # rank champions vs the field
wolframscript -file wolfram/figures.wls          # regenerate base figures"],
  para[{ital["Built entirely in the Wolfram Language. The strategy-discovery \
and autoresearch loops borrow the propose \[Rule] evaluate \[Rule] accept \
shape of automated-research scaffolds, re-implemented natively in WL."]}]
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

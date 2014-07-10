This is a program to take a graph and split it evenly into multiple subgraphs, the point being that large graphs could displayed across multiple pages. It inputs a rigid subset of Graphviz DOT files and outputs to that same rigid subset. It uses a simple pair-swapping hill-climbing algorithm, an algorithm of the simulated annealing category, and a branch and bound algorithm with linear relaxation that will eventually produce optimal results. (Somewhere in 30-70 elements, depending on the page size and edge density, the branch and bound disappears into infinity on my system. I believe the problem is NP-Complete, hence the difficulty of producing optimal results.)

There are a lot of unfinished elements here. Ultimately, Graphviz and characteristics of the data (much of which is stored as sample data) made this feel unproductive; this was no nice neat splits of the data, and Graphviz produces results with enough unnecessarily crossed lines and similar graphical nuisances to make me give up. Some dummy nodes that would let the pages be split in ways that aren't perfectly divisible would be nice; I believe adding too many would make the pages unbalanced, as they would tend to be pushed to one page, but might help make for aesthetically better splits. It might be nice to load real DOT files, instead of the woefully rigid subset I do; perhaps I should take it as an excuse to learn ANTLR or the parser included in Scala's main library. (JSON or XML would be easy but I don't have a need for them and I don't know of any preexisting graph formats in them.) I've thought about writing an all-JVM linear programming library that doesn't have GLPK's headhurting horribly imperative interface. (Part of the reason there's an abstraction layer for GLPK.) That would be a separate project, and I doubt I could make it even approach the efficency of GLPK. Good command-line arguments is important for polish; I've been editing main to change stuff like the number of annealing attempts to tune it for each problem, and to stop branch and bound from running on oversize problems.

To build, install sbt (Scala Build Tool) and run "sbt compile" in the main directory. I've been running it with `sbt "runMain Cherizo sample_data/movie1910s.dot 13 1910s_pages"`; if you want to run it as a normal program check out http://www.scala-sbt.org/sbt-native-packager/DetailedTopics/universal.html . (For testing, that should report that the optimal value is 484 (sum of edges coming from off-page for each node, or twice the number of edges crossing pages) and this copy returns ((0, 3, 16, 17, 19, 21, 25, 26, 29, 32, 33, 34, 38), (1, 2, 9, 10, 11, 12, 14, 18, 20, 22, 28, 30, 35), (4, 5, 6, 7, 8, 13, 15, 23, 24, 27, 31, 36, 37)) for the optimal solution, though there's probably other optimal solutions.

There is code for actors in its own branch; under states.dot, the actors code produced an optimal solution in 36,905 seconds (10 hours 15 minutes) and the original code produced an optimal solution in 10 hours 29 minutes; my gut guesses the relative speed on any one problem will depend way more on vagueries of order instead of which code is being used. (If GLPK were reenterant, I'd expect the actors to use all the cores on the machine; since it's not, CPU usage seemed pretty much the same.)

This file and all the source code is Copyright © 2014 David Starner.

    Cherizo Graph Splitter
    Copyright (C) 2014 David Starner

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


The sample data is all public information; I could claim a selection copyright on it, but that would be silly. I disclaim any copyright (unless otherwise noted on the file) on any file in the sample_data folder.

-- David Starner, Julian Date 2456842, 2014-07-03

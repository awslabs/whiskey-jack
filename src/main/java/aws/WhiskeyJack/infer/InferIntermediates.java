/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.infer;

import aws.WhiskeyJack.metadata.*;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.nodeviewerfx.*;
import java.util.*;
import java.util.concurrent.atomic.*;

public class InferIntermediates {
    TreeSet<CandidateSolution> workablePaths = new TreeSet<>(byWeight);
    Graph context;
    public void Scan(Graph g) {
        System.out.println("Inferring intermediates");
        context = g;
        if(g.typeMismatches.isEmpty()) {
            System.out.println("No type mismatches to fix!");
            return;
        }
        var candidates = new ArrayList<CandidateSolution>();
        var seen = new DTMap<Boolean>();
        var fixes = 0;
        for(var a: (List<Arc>) g.typeMismatches) {
            /* search for a path through all possible metanodes that minimizes
             * the weighted length of the path */
            System.out.println("Attempting to solve " + a.getMessage());
            var origin = a.oneEnd();
            var target = a.otherEnd();
            candidates.clear();
            candidates.add(new CandidateSolution(null, null, null, origin));
            seen.clear();
            seen.putExact(origin, Boolean.TRUE);
            while(workablePaths.isEmpty()) {
                var toScan = candidates.toArray(n -> new CandidateSolution[n]);
                if(toScan.length == 0) break;
                candidates.clear();
                for(var candidate: toScan) {
                    var src = candidate.out;
                    var ctr = new AtomicInteger(0);
                    System.out.println(candidate.indent()+"  Extending "+candidate.name+": "+src.dtString());
                    NodeLibrary.singleton.forEachNodeThatTakes(src.getDomain(), src.getType(),
                            mn -> {
                        ctr.incrementAndGet();
                        if(candidate.contains(mn)) {
                            System.out.println("\t\tSkipping loop " + mn.getName());
                            return;
                        }
                        var pin = mn.defaultPort(true);
                        var pout = mn.defaultPort(false);
                        var cs = new CandidateSolution(candidate, mn, pin, pout);
                        System.out.println(cs.indent() + cs);
                        if(cs.out.compatibleWith(target)) {
                            /* This gets us to the target */
                            System.out.println(cs.out.compatibleWith(target) + " " + cs.out + "->" + target);
                            workablePaths.add(cs);
                        } else if(seen.getExact(cs.out) == null) {
                            candidates.add(cs);
                            seen.putExact(cs.out, Boolean.TRUE);
                        } else
                            System.out.println(cs.indent() + " Skipping " + cs.out.getFullName());
                    });
                    if(ctr.intValue()==0) System.out.println(candidate.indent()+" (no candidates)");
                }
            }
            if(workablePaths.isEmpty()) System.out.println("No candidates");
            else {
                fixes++;
                System.out.println("Use " + workablePaths.first());
                a.delete();
                applyPath(origin, workablePaths.first(), target);
                if(context instanceof GraphView gv) gv.layoutNodes(false);
            }
            if(fixes>0) g.layoutNodes(false);
        }
        for(var p:(Collection<Port>)g.disconnectedPorts) {
            // TODO: do easy fixes on disconnected ports
        }
    }

    private void applyPath(Port origin, CandidateSolution s, Port target) {
        while(s != null)
            if(s.candidate != null) {
                var n = context.newNode(s.candidate);
                n.getPort(s.out.getName()).connectTo(target);
                target = n.getPort(s.in.getName());
                s = s.comesFrom;
            } else {
                origin.connectTo(target);
                break;
            }

    }

    @SuppressWarnings("unused")
    static private class CandidateSolution {
        final CandidateSolution comesFrom;
        final MetaNode candidate;
        final Port in, out;
        final float weight;
        final String name;
        final int depth;
        CandidateSolution(CandidateSolution cf, MetaNode c, Port i, Port o) {
            comesFrom = cf;
            candidate = c;
            in = i;
            out = o;
            weight = (c != null ? c.getWeight() : 0) + (cf != null ? cf.weight : 0);
            name = (cf != null ? cf.name + "-" : "") + (c != null ? c.getName() : "∅");
            depth = cf != null ? cf.depth + 1 : 0;
        }
        @Override
        public String toString() {
            return name + "[" + weight + "]";
        }
        public String indent() {
            return switch(depth) {
                default ->
                    "  ? ";
                case 0 ->
                    "  ";
                case 1 ->
                    "    ";
                case 2 ->
                    "      ";
                case 3 ->
                    "        ";
                case 4 ->
                    "          ";
                case 5 ->
                    "            ";

            };
        }
        private boolean contains(MetaNode mn) {
            return mn == candidate || comesFrom != null && comesFrom.contains(mn);
        }
    }
    private static final Comparator<CandidateSolution> byWeight
            = (a, b) -> a.weight > b.weight ? 1
                    : a.weight == b.weight ? 0
                            : -1;
}

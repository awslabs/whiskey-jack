/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code;

import aws.WhiskeyJack.QandA.*;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import static aws.WhiskeyJack.util.Utils.*;
import io.github.classgraph.*;
import java.util.*;
import java.util.function.*;

/**
 * This is the outermost driver of the compilation process. It drives the
 * resolution and invocation of the compilation plugins for all the different
 * paths and parts of the graph that need different compilation strategies.
 */
public class OverallCodeGenerationDriver {
    private String rootToken = "/"
                               + Question.question("buildsys").asString().toLowerCase()
                               + "/"; // TODO handle roots other than gradle
    public StrategyPath rootPath = new StrategyPath(rootToken, this);
    private Graph graph;
    private static CodeGeneratorPlugin plugins[];
    private Collection<Object> messages = new ArrayList<>();
    private Collection<Object> errors = new ArrayList<>();
    private OuterBuildController outerBuildController;
    public void compileEverything(Graph g) {
        graph = g;
        if(g.allOK) {
            System.out.println("\n__________________\nStarting build:");
            var obc = rootPath.findPlugin(OuterBuildController.class);
            outerBuildController = obc;
            System.out.println("outerBuildController " + obc + " " + rootToken);
            seperateDomains();
            if(domains.isEmpty()) messages.add("Nothing to run");
            else {
                obc.handleDomains(domains.values());
                if(errors.isEmpty() && Question.question("autorun").isTrue())
                    obc.runBuiltCode();
            }
        } else {
            g.typeMismatches.forEach((Consumer<Arc>) (a ->
                    errors.add(a.oneEnd().getName() + "->" + a.otherEnd().getName() + ": " + a.getMessage())));
            ((Collection<Port>)g.disconnectedPorts).forEach(
                    p->errors.add("Port "+p.getName()+" must be given a value"));
        }
        if(errors.isEmpty()) {
            if(!messages.isEmpty())
                g.note(messages);
        } else g.error("Can't run project", "Fix problems first:", errors);
    }
    public void message(Object... m) {
        messages.addAll(Arrays.asList(m));
    }
    public void error(Object... m) {
        errors.addAll(Arrays.asList(m));
    }
    public Graph getWholeGraph() {
        return graph;
    }
    private final Map<Domain, OneDomain> domains = new HashMap<>();
    private static void initPlugins() {
        var result = new ArrayList<>();
        /* This is somewhat carefully done to ensure that the plugin classes
         * are not initialized unless they are actually used */
        try(var scanResult = new ClassGraph()
                .enableAnnotationInfo()
                .scan()) {
            for(var ci: scanResult.getClassesWithAnnotation(GeneratesCodeFor.class))
                result.add(new CodeGeneratorPlugin(ci));
        }
        plugins = result.toArray(n -> new CodeGeneratorPlugin[n]);
    }
    private void seperateDomains() {
        /* One would think that the cast in the following line is redundent,
         * but errors get flagged without it. */
        graph.forEachNode((Consumer<Node>) (n ->
                domains.computeIfAbsent(n.getDomain(), d ->
                        new OneDomain(this, d)).nodes.add(n)));
    }

    public class StrategyPath {
        private String path;
        private final OverallCodeGenerationDriver ocg;
        StrategyPath(String p, OverallCodeGenerationDriver d) {
            System.out.println("new StrategyPath(\"" + p + "\");");
            path = p;
            ocg = d;
        }
        public StrategyPath append(String tail) {
            var p = path == null ? tail : (path + tail).replaceAll("//+", "/");
            if(!p.startsWith("/")) p = "/" + p;
            return new StrategyPath(p, ocg);
        }
        public <T> T findPlugin(Class<T> clazz) {
            if(plugins == null) initPlugins();
            CodeGeneratorPlugin best = null;
            var tname = clazz == null ? null : clazz.getName();
            var bestScore = -1;
            System.out.println("findPath " + path + " " + clazz);
            String tail = "";
            for(var p: plugins) {
//                System.out.println("  FP " + target + ": " + p.glob.toString() + " " + p.ci);
                String ltail;
                if((ltail = p.matches(path)) != null) {
                    if(tname != null) {
                        var found = false;
                        for(var s: p.getInterfaces())
                            if(s.equals(tname)) {
                                found = true;
                                break;
                            }
                        if(!found) continue;
                    }
                    var score = ltail.length(); // p.score();
                    if(score > bestScore) {
                        bestScore = score;
                        best = p;
                        tail = ltail;
                    }
                }
            }
            path = tail;
            if(best != null)
                System.out.println("\t => Pattern " + best.glob.toString() + "  tail: " + tail);
            return best == null ? null : (T) best.getGenerator(ocg);
        }
        @Override
        public String toString() {
            return path;
        }
    }

    private static class CodeGeneratorPlugin {
        private final ClassInfo ci;
        private Object generator;
        private final Glob glob;
        private final int score;
        private final String[] interfaces;

        public CodeGeneratorPlugin(ClassInfo ci) {
            this.ci = ci;
            var globPattern = "No Pattern";
            System.out.println("Class " + ci.getName());
            var matches = ci.getAnnotationInfo(GeneratesCodeFor.class);
            if(matches == null)
                System.out.println("No @Matches annotation on " + ci.getName());
            else
                for(var param: matches.getParameterValues())
                    if("value".equals(param.getName())) {
                        globPattern = String.valueOf(param.getValue());
                        System.out.println("  adding " + param.getValue() + " -> " + ci.getName());
                    }
            List<String> infs = new ArrayList<>(4);
            for(var i: ci.getInterfaces())
                infs.add(i.getName());
            interfaces = infs.toArray(n -> new String[n]);
            System.out.println("CGP " + globPattern + " " + deepToString(interfaces));
            System.out.println("   Interfaces: " + Utils.deepToString(interfaces));
            glob = Glob.compile(globPattern);
            score = globPattern.length();
        }
        public CodeGeneratorPlugin(String pattern, DomainGenerationController gc, String... infs) {
            ci = null;
            glob = Glob.compile(pattern);
            score = pattern.length();
            generator = gc;
            interfaces = infs;
        }
        public String matches(String s) {
            return glob.matches(s);
        }
        public int score() {
            return score;
        }
        @SuppressWarnings({"PMD.UnnecessaryLocalBeforeReturn", "PMD.AvoidCatchingThrowable", "UseSpecificCatch"})
        public Object getGenerator(OverallCodeGenerationDriver ocg) throws IllegalStateException {
            if(generator == null) try {
                generator = Class.forName(ci.getName()).getConstructor().newInstance();
                if(generator instanceof OuterBuildController obc)
                    ocg.outerBuildController = obc;
                if(generator instanceof needsCodeGenerator scg)
                    scg.setCodeGenerator(ocg);
                if(generator instanceof needsOuterBuildController obc)
                    obc.setOuterBuildController(ocg.outerBuildController);
            } catch(Throwable t) {
                getUltimateCause(t).printStackTrace(System.out);
                throw new IllegalStateException(t.getMessage(), t);
            }
            return generator;
        }
        public String[] getInterfaces() {
            return interfaces;
        }
    }

    public interface needsCodeGenerator {
        public void setCodeGenerator(OverallCodeGenerationDriver cg);
    }

    public interface needsOuterBuildController {
        public void setOuterBuildController(OuterBuildController cg);
    }

}

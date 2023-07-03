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
    private String rootToken = "/gradle/"; // TODO handle roots other than gradle
    private Graph graph;
    private static CodeGeneratorPlugin plugins[];
    private Collection<Object> messages = new ArrayList<>();
    private Collection<Object> errors = new ArrayList<>();
    private OuterBuildController outerBuildController;
    public void Scan(Graph g) {
        graph = g;
        if(g.allOK) {
            var obc = findPlugin(rootToken, OuterBuildController.class);
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
            errors.addAll(g.otherErrors);
        }
        if(errors.isEmpty()) {
            if(!messages.isEmpty())
                g.note(messages);
            // TODO: compile and run the generated code
        } else g.error("Can't run project", "Fix problems first:", errors);
    }
    public void message(Object... m) {
        for(var M:m) messages.add(M);
    }
    public void error(Object... m) {
        for(var M:m) errors.add(M);
    }
    public Graph getWholeGraph() { return graph; }
    private final Map<Domain, OneDomain> domains = new HashMap<>();
    private void initPlugins() {
        var result = new ArrayList<CodeGeneratorPlugin>();
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
    public <T> T findPlugin(String target, Class<T> clazz) {
        if(plugins == null) initPlugins();
        CodeGeneratorPlugin best = null;
        var tname = clazz == null ? null : clazz.getName();
        var bestScore = -1;
        for(var p: plugins) {
            System.out.println("FP "+target+": "+p.glob.toString()+" "+p.ci);
            if(p.matches(target)) {
                if(tname != null) {
                    var found = false;
                    for(var s: p.getInterfaces())
                        if(s.equals(tname)) {
                            found = true;
                            break;
                        }
                    if(!found) continue;
                }
                var score = p.score();
                if(score > bestScore) {
                    bestScore = score;
                    best = p;
                }
            }
        }
        return best == null ? null : (T) best.getGenerator();
    }
    private void seperateDomains() {
        /* One would think that the cast in the following line is redundent,
         * but errors get flagged without it. */
        graph.forEachNode((Consumer<Node>) (n ->
                domains.computeIfAbsent(n.getDomain(), d ->
                        new OneDomain(this, d, rootToken)).nodes.add(n)));
    }

    private class CodeGeneratorPlugin {
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
            System.out.println("CGP "+globPattern+" "+deepToString(interfaces));
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
        public boolean matches(String s) {
            return glob.matches(s);
        }
        public int score() {
            return score;
        }
        @SuppressWarnings({"PMD.UnnecessaryLocalBeforeReturn", "PMD.AvoidCatchingThrowable", "UseSpecificCatch"})
        public Object getGenerator() throws IllegalStateException {
            if(generator == null) try {
                generator = Class.forName(ci.getName()).getConstructor().newInstance();
                if(generator instanceof OuterBuildController obc)
                    outerBuildController = obc;
                if(generator instanceof needsCodeGenerator scg)
                    scg.setCodeGenerator(OverallCodeGenerationDriver.this);
                if(generator instanceof needsOuterBuildController obc)
                    obc.setOuterBuildController(outerBuildController);
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

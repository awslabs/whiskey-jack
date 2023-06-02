/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code;

import aws.WhiskeyJack.util.Utils;
import aws.WhiskeyJack.nodegraph.Arc;
import aws.WhiskeyJack.nodegraph.Graph;
import aws.WhiskeyJack.nodegraph.Node;
import aws.WhiskeyJack.nodegraph.Domain;
import io.github.classgraph.*;
import java.io.*;
import java.util.*;
import java.util.function.*;

public class CodeGenerator {
    Graph context;
    static CodeGeneratorPlugin plugins[];
    Collection<Object> messages = new ArrayList<>();
    public void Scan(Graph g) {
        context = g;
        if(g.allOK) {
            seperateDomains();
            domains.values().forEach(d -> {
                d.prescan();
            });
            domains.values().forEach(d -> d.generate());
            domains.values().forEach(d -> d.close());
        } else {
            Collection<String> errors = new ArrayList<>();
            g.typeMismatches.forEach((Consumer<Arc>) (a ->
                    errors.add(a.oneEnd().getName() + "->" + a.otherEnd().getName() + ": " + a.getMessage())));
            errors.addAll(g.otherErrors);
            g.error("Can't run project", "Fix problems first:", errors);
        }
        if(!messages.isEmpty())
            g.note(messages);
    }
    private final Map<Domain, OneDomain> domains = new HashMap<>();
    private void initPlugins() {
        var result = new ArrayList<CodeGeneratorPlugin>();
        try(var scanResult = new ClassGraph()
                .enableAnnotationInfo()
                .scan()) {
            for(var ci: scanResult.getClassesWithAnnotation(Matches.class))
                result.add(new CodeGeneratorPlugin(ci));
        }
        plugins = result.toArray(n -> new CodeGeneratorPlugin[n]);
    }
    public <T> T findPlugin(String pattern, Class<T> clazz) {
        if(plugins == null) initPlugins();
        CodeGeneratorPlugin best = null;
        var tname = clazz == null ? null : clazz.getName();
        var bestScore = -1;
        for(var p: plugins)
            if(p.matches(pattern)) {
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
        return best == null ? null : (T) best.getGenerator();
    }
    private void seperateDomains() {
        /* One would think that the cast in the following line is redundent,
         * but errors get flagged without it. */
        context.forEachNode((Consumer<Node>) (n ->
                domains.computeIfAbsent(n.getDomain(), d -> new OneDomain(d)).nodes.add(n)));
    }

    private class OneDomain implements Closeable {
        final DomainGenerationController generator;
        final Domain domain;
        String targetToken;
        final List<Node> nodes = new ArrayList<>();
        CodeTarget out;
        OneDomain(Domain d) {
            var framework
                    = // TODO the real thing
                    d == Domain.device ? "greengrass"
                            : d == Domain.cloud ? "apprunner"
                                    : d == Domain.browser ? "jquery"
                                            : "nothing";
            var style = "app";
            var language = "java";
            targetToken = "/" + d + "/" + framework + "/" + style + "/" + language;
            generator = findPlugin(targetToken, DomainGenerationController.class);
            domain = d;
            out = generator.makeOutput(d);
            messages.add("Generating " + out.getPath());
            messages.add(" target token: " + targetToken);
        }
        void prescan() {
            generator.prescan(messages);
        }
        void generate() {
            messages.add(domain + ": " + nodes.size() + " nodes");
            out.comment("Code for domain " + domain);
            generator.generate(nodes, out);
        }
        @Override
        public void close() {
            generator.close();
            Utils.close(out);
        }
    }
}

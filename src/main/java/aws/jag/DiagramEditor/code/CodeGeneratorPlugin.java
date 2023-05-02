/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.code;

import aws.jag.DiagramEditor.util.*;
import io.github.classgraph.*;
import java.util.*;

public class CodeGeneratorPlugin {
    private final ClassInfo ci;
    private GenerateCode generator;
    private final Glob glob;
    private final int score;
    private final String[] interfaces;
    public CodeGeneratorPlugin(ClassInfo ci) {
        this.ci = ci;
        var globPattern = "No Pattern";
        System.out.println("Class " + ci.getName());
        var matches = ci.getAnnotationInfo(Matches.class);
        if(matches == null) {
            System.out.println("No @Matches annotation on " + ci.getName());
        } else {
            for(var param: matches.getParameterValues())
                if("value".equals(param.getName())) {
                    globPattern = String.valueOf(param.getValue());
                    System.out.println("  adding " + param.getValue() + " -> " + ci.getName());
                }
        }
        List<String> infs = new ArrayList<>(4);
        for(var i:ci.getInterfaces())
            infs.add(i.getName());
        interfaces = infs.toArray(n->new String[n]);
        System.out.println("   Interfaces: "+Utils.deepToString(interfaces));
        glob = Glob.compile(globPattern);
        score = globPattern.length();
    }
    public CodeGeneratorPlugin(String pattern, GenerateCode gc, String... infs) {
        ci = null;
        glob = Glob.compile(pattern);
        score = pattern.length();
        generator = gc;
        interfaces = infs;
    }
    public boolean matches(String s) {
        return glob.matches(s);
    }
    public int score() { return score; }
    @SuppressWarnings({"PMD.UnnecessaryLocalBeforeReturn", "PMD.AvoidCatchingThrowable", "UseSpecificCatch"})
    public GenerateCode getGenerator() throws IllegalStateException {
        if(generator == null) try {
            generator = (GenerateCode) Class.forName(ci.getName()).getConstructor().newInstance();
        } catch(Throwable t) {
            t.printStackTrace(System.out);
            throw new IllegalStateException(t.getMessage(), t);
        }
        return generator;
    }
    public String[] getInterfaces() { return interfaces; }
}

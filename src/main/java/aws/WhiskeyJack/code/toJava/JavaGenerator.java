/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code.toJava;

import aws.WhiskeyJack.code.*;
import aws.WhiskeyJack.code.OverallCodeGenerationDriver.StrategyPath;
import aws.WhiskeyJack.exl.*;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import java.io.*;
import java.nio.file.*;
//import java.nio.file.*;
import java.util.*;
import java.util.function.*;

@GeneratesCodeFor("*/java")
public class JavaGenerator implements DomainGenerationController,
    OverallCodeGenerationDriver.needsOuterBuildController {
    Set<String> imports = new HashSet<>();
    Set<String> libraries = new HashSet<>();
    Set<String> dependencies = new HashSet<>();
    String className;
    String packageName = "ade.gen";
    Path outName;
    DomainCode code;
    @Override
    public void generate(DomainCode c) {
        var codeRootDirectory = context
            .getCodePartDirectory(c.getDomain(), "code")
            .resolve(packageName);
        try {
            Files.createDirectories(codeRootDirectory);
        } catch(IOException ex) {
        }
        outName = codeRootDirectory.resolve(c.getDomain() + ".java");
        var encoder = new VerboseDump() {
            @Override
            public void fileHeader(DomainCode dc) {
                super.fileHeader(dc);
                append("package ").append(packageName).append(";\n");
                imports.forEach(i -> append("import ").append(i).append(";\n"));
                append("class " + c.getDomain() + " {\n");
            }
            @Override
            public void fileEnder(DomainCode dc) {
                append("}");
            }
            @Override
            public void declareGlobal(Expression e) {
                toTab(1);
                append(e);
                append(";\n");
            }
            public void appendSetup(Expression e) {
                toCol(1);
                appendStatement(e, 1);
            }
            @Override
            protected void appendStatement(Expression e, int indent) {
                if(e != null) {
                    var op = e.getOperator();
                    if(op == Vocabulary.BLOCK) {
                        append('{');
                        for(var arg: e.asArray())
                            appendStatement(arg, indent);
                        toTab(indent - 1);
                        append('}');
                        return;
                    }
                    toTab(indent);
                    if(op == Vocabulary.IF) {
                        var cond = e.getOK(0);
                        var then = e.getOK(1);
                        var elze = e.getOK(2);
                        append("if(");
                        append(cond, indent + 1, 0);
                        append(") ");
                        if(then == null) append("{}");
                        else
                            appendStatement(then.endsWithIf() ?
                                DomainCode.block(then) : then,
                                indent + 1);
                        if(elze != null) {
                            toTab(indent);
                            append("else ");
                            appendStatement(elze, indent + 1);
                        }
                    } else {
                        append(e, indent, 0);
                        append(';');
                    }
                }
            }
            @Override
            public void append(Expression e, int indent, int outerprior) {
                if(e != null) {
                    var op = e.getOperator();
                    if(op == Vocabulary.NEW) {
                        append("new ");
                        append(e.get(0));
                        var sz = e.size();
                        append('(');
                        for(var i = 1; i < sz; i++) {
                            if(i > 1) append(", ");
                            append(e.get(i));
                        }
                        append(')');
                    } else super.append(e, indent, outerprior);
                }
            }
        };
        try {
            encoder.to(outName);
            encoder.append(c);
            encoder.close();
            Exec.edit(outName);
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
        }
        code = c;
        System.out.println("Generating java code for " + c.toString());
    }
    @Override
    public void setStrategyPath(StrategyPath p) {
//        System.out.println("  java generator followon path "+p);
//        strategyPath = p;
    }
    private static String typeFrom(Type e) {
        if(e == Type.number) return "double";
        if(e == Type.string) return "String";
        if(e == Type.bool) return "boolean";
        if(e == Type.object) return "Object";
        if(e == Type.tuple) return "Map";
        return "void";
    }
    private static String typeFrom(Expression e) {
        if(e == null) return "unknown";
        var t = e.getType();
        if(t == Type.tuple) {
            // (new (invoke T args))
            Expression ne = e.get(0);
            if(ne.getOperator() == Vocabulary.INVOKE)
                return ne.get(0).toString(); // TODO: pretty hacky
        }
        return typeFrom(t);
    }
    private static void forAllBaseExpressionsIn(Expression e, Consumer<Expression> func) {
        if(e != null)
            if(e.getOperator() == Vocabulary.BLOCK)
                for(var e2: e.asArray())
                    forAllBaseExpressionsIn(e2, func);
            else func.accept(e);
    }
    OuterBuildController context;
    @Override
    public void setOuterBuildController(OuterBuildController cg) {
        System.out.println("setOuterBuildController " + cg);
        assert cg != null;
        context = cg;
    }

}

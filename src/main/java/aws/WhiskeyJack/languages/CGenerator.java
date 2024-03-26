/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.languages;

import aws.WhiskeyJack.code.*;
import aws.WhiskeyJack.code.OverallCodeGenerationDriver.StrategyPath;
import aws.WhiskeyJack.exl.*;
import aws.WhiskeyJack.exl.DomainCode.FunctionInfo;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import java.io.*;
import java.nio.file.*;
//import java.nio.file.*;
import java.util.*;
import java.util.function.*;

@GeneratesCodeFor("*/c")
public class CGenerator implements DomainGenerationController,
    OverallCodeGenerationDriver.needsOuterBuildController {
    Set<String> includes = new HashSet<>();
    Set<String> externs = new HashSet<>();
    Set<String> libraries = new HashSet<>();
    Set<String> dependencies = new HashSet<>();
    Path outName;
    DomainCode code;
    @Override
    public void generate(DomainCode c) {
        includes.add("stdio.h");
        includes.add("sys/types.h");
        adjustForC(c);
        var codeRootDirectory = context
            .getCodePartDirectory(c.getDomain(), "code");
        try {
            Files.createDirectories(codeRootDirectory);
        } catch(IOException ex) {
        }
        outName = codeRootDirectory.resolve(c.getDomain() + ".c");
        var encoder = new VerboseDump() {
            @Override
            public void append(DomainCode.FunctionInfo e) {
                if(e.isUsed()) {
                    toTab(0).append(e.returnType.getName())
                        .append(' ')
                        .append(e.toString());
                    append('(');
                    var first = true;
                    for(var arg: e.args) {
                        if(first) first = false;
                        else append(',');
                        append(arg.getType().toString());
                        append(' ');
                        append(arg, 0, 2);
                    }
                    append(") ");
                    var b = e.body;
                    if(b == null || b.getOperator() != Vocabulary.BLOCK)
                        b = DomainCode.block(b);
                    appendStatement(b, 1);
                    toTab(0);
                }
//        else toCol(0).startComment().append("Unused: ").append(e.name).endComment();
            }
            @Override
            public void appendDeclaration(DomainCode.DeclarationInfo decl) {
                // if(decl.isFinal) append("final ");
                append(typeFrom(decl.type));
                append(' ');
                append(decl.name);
                if(decl.initialValue != null) {
                    append(" = ");
                    append(decl.initialValue);
                }
            }
            @Override
            public void fileHeader(DomainCode dc) {
                super.fileHeader(dc);
                comment("Generated C code for " + c.getDomain() + " ");
                includes.forEach(i ->
                    append("include <").append(i).append(">\n"));
                externs.forEach(i -> append(i).append("\n"));
            }
            @Override
            public void fileEnder(DomainCode dc) {
                comment("EOF");
            }
            @Override
            public void declareGlobal(Expression e) {
                toTab(0);
                append("static ");
                append(e);
                append(";\n");
            }
            @Override
            public void appendSetup(Expression e) {
                toCol(0);
                append("static void __attribute__((constructor)) setup() ");
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
                            appendStatement(then.endsWithIf()
                                ? DomainCode.block(then) : then,
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
                    if(op == Vocabulary.INVOKE) {
                        var method = e.get(0);
                        if(method==addressOf) {
                            append("&");
                            append(e.getOK(1),indent,outerprior);
                            return;
                        }
                    }
                    super.append(e, indent, outerprior);
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
    private void adjustForC(DomainCode c) {
        System.out.println("BEGIN adjustForC");
        var λ = new ArrayList<DomainCode.FunctionInfo>();
        c.rewriteAllCode(e -> {
            var op = e.getOperator();
            if(op == Vocabulary.NEW) {
                var ctor = e.get(0);
                var t = ctor;
                while(!t.isLeaf() && !t.isEmpty()) t = t.get(0);
                var fn = t + "_ctor";
                var σ = new StringBuilder();
                for(int i = 1; i < e.size(); i++) {
                    if(!σ.isEmpty()) σ.append(", ");
                    σ.append(typeFrom(e.get(i)));
                }
                externs.add("extern " + t + "* " + fn + "(" + σ + ");");
                var η = DomainCode.invoke(DomainCode.ide(fn));
                for(int i = 1; i < e.size(); i++)
                    η.add(e.get(i));
                return η;
            } else if(op == Vocabulary.RARROW) {
                var λname = "lambda_" + e.size() + 1;
                System.out.println("λ " + λname + " " + e.getOK(1));
                λ.add(c.new FunctionInfo(λname, e.getOK(1), e.getOK(0)));
                return DomainCode.invoke(addressOf, DomainCode.ide(λname));
            } else return e;
        });
        λ.forEach(l -> c.add(l));
        System.out.println("END adjustForC");
    }
    private final Expression addressOf = DomainCode.ide("addressOf");
    @Override
    public void setStrategyPath(StrategyPath p) {
//        System.out.println("  java generator followon path "+p);
//        strategyPath = p;
    }
    private static String typeFrom(Type e) {
        if(e == Type.number) return "long double";
        if(e == Type.string) return "char*";
        if(e == Type.bool) return "int";
        if(e == Type.object) return "void*";
        if(e == Type.tuple) return "CMAP*";
        return "void*";
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

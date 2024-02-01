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
//import java.nio.file.*;
import java.util.*;
import java.util.function.*;
import java.util.regex.*;

@GeneratesCodeFor("*/java")
public class JavaGenerator implements DomainGenerationController,
    OverallCodeGenerationDriver.needsOuterBuildController {
    CodeTarget out;
    Set<String> imports = new HashSet<>();
    Set<String> libraries = new HashSet<>();
    Set<String> dependencies = new HashSet<>();
    String className;
    String packageName = "ade.gen";
    @Override
    public void generate(List<Node> nodes, CodeTarget out) {
        this.out = out;
        if(false) {
            // TODO aws.WhiskeyJack.code.toJava.JavaGenerator.generate Not implemented
            System.out.println("  Whoo! " + this);
            var m = Pattern.compile("^([^.]*)\\..*").matcher(out.getPath().getFileName().toString());
            className = m.group(m.matches() ? 1 : 0);
            nodes.forEach((Node n) -> {
                n.sidecar(NodeGenerationInfo.class);
            });
            nodes.forEach((Node n) -> {
                imports.addAll(n.getStringListProp("import"));
                libraries.addAll(n.getStringListProp("library"));
                dependencies.addAll(n.getStringListProp("dependencies"));
            });
            out.append("package ").append(packageName).append(";\n");
            imports.forEach((String im) -> {
                out.append("import ").append(im).append(";\n");
            });
            out.comment(libraries);
            out.append("public class ")
                .append(className)
                .append(" {\n")
                .indent();
            nodes.forEach((Node n) -> {
                Collection<String> header = new ArrayList<>();
                var info = n.sidecar(NodeGenerationInfo.class);
                header.add("Node " + n.getName() + " (" + n.getUid() + ")");
                n.forEachPort((Consumer<Port>) (p -> {
                    header.add((p.isInputSide() ? "in  " : "out ") + p.getType()
                               + '\t' + p.getName()
                               + '\t' + p.getValue()
                               + '\t' + p.isConnected());
                }));
                info.inputs.forEach(input -> header.add("input " + input));
                info.outputs.forEach(output -> header.add("output " + output));
                context.message(n.getUid());
                out.ln().comment(header);
                var code = n.getStringProp("code", null);
                var state = n.getStringProp("state", null);
                var callback = n.getStringProp("callback", null);
                if(state != null)
                    forAllBaseExpressionsIn(parse(state),
                        ist0 -> {
                        var ist = info.rewritePorts(ist0);
                        var t = ist.getOperator();
                        if(t == Vocabulary.DECLAREASSIGN || t == Vocabulary.DECLAREASSIGNFINAL) {
                            flushPendingInit();
                            var name = ist.get(0);
                            var exp = ist.get(1);
                            out.append("private ");
                            if(t == Vocabulary.DECLAREASSIGNFINAL)
                                out.append("final ");
                            out.append(typeFrom(exp))
                                .append(' ')
                                .append(name)
                                .append(" = ")
                                .append(exp)
                                .append(';')
                                .nl();
                        } else pendingInit.add(ist);
                    });
                if(callback != null)
                    forAllBaseExpressionsIn(parse(callback),
                        ist -> pendingInit.add(info.rewritePorts(ist)));
                flushPendingInit();
                info.inputs.forEach(input -> {
                    var nm = NodeGenerationInfo.uniqueName(input);
                    var type = typeFrom(input.getType());
                    out.nl().append("private ").append(type).append(' ')
                        .append(nm).append("; // TODO: initial value\n");
                    out.append("private void ").append(nm)
                        .append('(').append(type).append(" v) ")
                        .openBrace()
                        .append("if(v != ").append(nm)
                        .append(") ").openBrace()
                        .append(nm).append(" = v;").nl()
                        .append("compute_").append(n.getUid()).append("();\n")
                        .closeBrace().closeBrace();
                });
                info.outputs.forEach(output -> {
                    var nm = NodeGenerationInfo.uniqueName(output);
                    var type = typeFrom(output.getType());
                    out.nl().append("private ").append(type).append(' ')
                        .append(nm).append("; // TODO: initial value\n");
                    out.append("private void ").append(nm)
                        .append('(').append(type).append(" v) ")
                        .openBrace()
                        .append("if(v != ").append(nm)
                        .append(") ").openBrace()
                        .append(nm).append(" = v;").nl();
                    output.forEachArc(arc -> {
                        var receiver = arc.otherEnd(output);
                        var rnm = NodeGenerationInfo.uniqueName(receiver);
                        out.append(rnm).append("(v);\n");
                    });
                    out.closeBrace().closeBrace();
                });
                if(!Utils.isEmpty(code)) {
                    out.append("void ").append("compute_" + n.getUid()).append("() ").openBrace();
                    var parsed = info.rewritePorts(parse(code));
                    out.append(clean(parsed))
                        .append(";");
                    out.closeBrace();
                }
                var errors = info.getErrors();
                if(errors != null) {
                    out.comment(errors);
                    context.error(errors);
                }
            });
            out.nl().append("public static void main(String [] args)")
                .openBrace()
                .append("var v = new ").append(className).append("();")
                .closeBrace();
            out.setIndent(0).append("}\n");
            // generatePOM();
            context.generateJavaPartBuild(out.getCodeRootDirectory(),
                packageName + "." + className, dependencies);

            nodes.forEach(n -> n.removeSidecar(NodeGenerationInfo.class));
//        return error;
        }
    }
//    private StrategyPath strategyPath;
    @Override
    public void setStrategyPath(StrategyPath p) {
//        System.out.println("  java generator followon path "+p);
//        strategyPath = p;
    }
    private Expression parse(String s) {
        try {
            if(s.indexOf(';') >= 0)
                s = "{" + s + ";}";
            return new Parser(new Tokenizer(s)).expression();
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
            var msg = List.of("Error parsing", s, ex.toString());
            out.comment(msg);
            context.message(msg);
            return clean(Expression.of(Token.string(s)));
        }
    }
    static Expression clean(Expression e) {
        return e == null || e.getOperator() != Vocabulary.BLOCK || e.size() != 1
            ? e
            : clean(e.get(0));
    }
    private final Collection<Expression> pendingInit = new ArrayList<>();
    private void flushPendingInit() {
        if(!pendingInit.isEmpty()) {
            out.nl().append(Expression.of(Vocabulary.BLOCK, pendingInit)).nl();
            pendingInit.clear();
        }
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
//    void generatePOM() {
//
//        try(var pom = Files.newBufferedWriter(out.getCodeRootDirectory().resolve("pom.xml"),
//                StandardOpenOption.CREATE,
//                StandardOpenOption.TRUNCATE_EXISTING)) {
//            pom.append(
//                    """
//<?xml version="1.0" encoding="UTF-8"?>
//<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
//    <modelVersion>4.0.0</modelVersion>
//    <groupId>ade.exp</groupId>
//    <artifactId>adeWhatever</artifactId>
//    <version>1.0-SNAPSHOT</version>
//    <packaging>jar</packaging>
//    <properties>
//        <maven.compiler.source>21</maven.compiler.source>
//        <maven.compiler.target>21</maven.compiler.target>
//    </properties>
//                       """);
//            if(!dependencies.isEmpty()) {
//                pom.append("    <dependencies>\n");
//                dependencies.forEach(d -> {
//                    try {
//                        pom.append("\t<dependency>\n\t  " + d + "\n\t</dependency>\n");
//                    } catch(IOException ioe) {
//                    }
//                });
//                pom.append("    </dependencies>\n");
//            }
//            pom.append("</project>\n");
//        } catch(IOException ex) {
//            out.comment(ex.toString());
//        }
//    }
    @Override
    public CodeTarget makeOutput() {
        assert context != null;
//        var src = context.getWholeGraph().getSrc();
//        System.out.println("SRC " + src + "; " + src.getFileName());
        return new JavaTarget(context, packageName);
    }
    OuterBuildController context;
    @Override
    public void setOuterBuildController(OuterBuildController cg) {
        System.out.println("setOuterBuildController " + cg);
        assert cg != null;
        context = cg;
    }

    public static class NodeGenerationInfo {
        private final Map<String, Expression> portValues = new HashMap<>();
        Collection<Port> inputs = new ArrayList<>();
        Collection<Port> outputs = new ArrayList<>();

        public NodeGenerationInfo(Node n) {
            n.forEachPort((Consumer<Port>) p -> {
                var t = p.getType();
                var k = p.getName();
                if(p.isConnected()) {
                    (p.isInputSide() ? inputs : outputs).add(p);
                    portValues.put(k, Expression.of(Token.identifier(uniqueName(p))));
                } else {
                    var nv = p.getValue();
                    System.out.println("Constant port " + p.getFullName() + " = " + nv);
                    var v = switch(nv) {
                        case String str ->
                            t == Type.string
                            ? Expression.of(Token.string(str))
                            : parse(str);
                        case Expression ev ->
                            ev;
                        case Number num ->
                            Expression.of(Token.number(num));
                        case null ->
                            Expression.of(t == Type.string ? emptyString : Vocabulary.NULL);
                        default ->
                            Expression.of(Token.string(String.valueOf(nv)));
                    };
                    System.out.println("rewrite " + k + " to " + v);
                    portValues.put(k, v);
                }
            });
        }
        Expression rewritePorts(Expression e) {
            return e.rewrite((Expression exNode) -> {
                var op = exNode.getOperator();
                if(op.isIdentifier()) {
                    System.out.println("Found Identifier " + exNode);
                    var foundValue = portValues.get(op.getBody());
                    if(foundValue != null) {
                        System.out.println("  with value " + foundValue);
                        return foundValue;
                    }
                }
                return exNode;
            });
        }
        private Expression parse(String s) {
            try {
                if(s.indexOf(';') >= 0)
                    s = "{" + s + ";}";
                return new Parser(new Tokenizer(s)).expression();
            } catch(IOException ex) {
                ex.printStackTrace(System.out);
                if(errors == null) errors = new ArrayList<>();
                errors.add("Error parsing " + s);
                errors.add(ex.toString());
                return JavaGenerator.clean(Expression.of(Token.string(s)));
            }
        }
        private List<String> errors;
        public Collection<String> getErrors() {
            return errors;
        }
        static String uniqueName(Port p) {
            return p == null ? "NULLPORT" : p.within.getUid() + "_" + p.getName();
        }
        private static final Token emptyString = Token.string("");
    }

}

/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code.greengrass;

import aws.WhiskeyJack.QandA.*;
import aws.WhiskeyJack.code.*;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import static aws.WhiskeyJack.util.Utils.*;
import java.io.*;
import java.lang.reflect.*;
import java.util.*;

@GeneratesCodeFor("*/greengrass/*")
public class GGBuildController implements DomainGenerationController,
        OverallCodeGenerationDriver.needsOuterBuildController {
    private final List manifests = new ArrayList();
//    String description = "xyzzy";
    OuterBuildController context;
    DomainGenerationController subController;
//    private Path rootGenerationDirectory;
    private GGTarget out;
    @Override
    public void close() {
        subController.close();
        out.dumpComments();
        var root = m(
        "RecipeFormatVersion", "2020-01-25",
        "ComponentName", Question.question("name").asString(),
        "ComponentVersion", Question.question("version").asString(),
        "ComponentDescription", Question.question("description").asString(),
        "ComponentPublisher", Question.question("author").asString(),
        "Manifests", manifests);
//        root.put("ComponentConfiguration", "");
//  DefaultConfiguration:
//    Message: "World"
//  - Platform:
//      os: all
//    Artifacts:
//      - URI: "s3://BUCKET_NAME/COMPONENT_NAME/COMPONENT_VERSION/HelloWorld-1.0.0.jar"
//    Lifecycle:
//      Run: "java -cp {artifacts:path}/HelloWorld-1.0.0.jar com.hello.App {configuration:/Message}"%   
        try {
            DataIO.yaml.write(root, out.getWriter());
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
        }
    }
    @Override
    public void generate(List<Node> nodes, CodeTarget target) {
        subController.generate(nodes, subController.makeOutput());
        nodes.forEach(n -> {
            var cname = n.getStringProp("cname", null);
            System.out.println("GG generate " + n.getName() + " cn " + cname);
            if(!isEmpty(cname)) manifests.add(cname);
        });
    }
    private Object dup(Node context, Object o) {
        return switch(o) {
            case null ->
                o;
            case Collection c -> {
                var n = new ArrayList();
                c.forEach(e -> n.add(dup(context, e)));
                yield (n);
            }
            case String s ->
                s.startsWith("$") ? eval(context, s) : s;
            case Map m -> {
                var n = new HashMap();
                m.forEach((k, v) -> n.put(k, dup(context, v)));
                yield n;
            }
            default -> {
                if(o.getClass().isArray()) {
                    var n = new ArrayList();
                    var limit = Array.getLength(o);
                    for(var i = 0; i < limit; i++)
                        n.add(dup(context, Array.get(o, i)));
                    yield n;
                } else yield o;
            }
        };
    }
    /* evaluate simple path references a.b.c */
    private String eval(Node context, String s) {
        var path = (s.startsWith("$") ? s.substring(1) : s).split("\\.");
        var len = path.length - 1;
//        System.out.println("GG path "+join("><", path));
        for(var i = 0; i < len; i++) {
            var next = context.getPort(path[i]);
            if(next.isConnected()) {
                var a = next.getArc(0);
                context = a.otherEnd(next).within;
            } else
                throw new IllegalArgumentException(path[i] + " not connected, in " + s);
//            System.out.println(path[i]+" "+s+" -> "+context);
        }
        var result = context.getPort(path[len]);
//        System.out.println("GG resolved "+s+" -> "+result);
        if(result == null)
            throw new IllegalArgumentException(context.getName() + "." + path[len] + " not defined");
        if(result.isConnected() || result.isCode() || result.getValue() == null)
            throw new IllegalArgumentException(context.getName() + "." + path[len] + " must be a literal constant");
        return result.getValue().toString();
    }
    private OverallCodeGenerationDriver.StrategyPath strategyPath;
    @Override
    public void setStrategyPath(OverallCodeGenerationDriver.StrategyPath p) {
        System.out.println("  GG generator followon path " + p);
        strategyPath = p;
        subController = p.findPlugin(DomainGenerationController.class);
        System.out.println("GG subcontroller = "+subController);
    }
    @Override
    public void setOuterBuildController(OuterBuildController cg) {
        context = cg;
    }
    @Override
    public CodeTarget makeOutput() {
        return out = new GGTarget(context);
    }
    @Override
    public void prescan() {
        if(subController==null)
            context.error("Couldn't resolve code generator for "+strategyPath);
        else subController.prescan();        
    }
    private static Map m(Object... kv) {
        var ret = new LinkedHashMap();
        var limit = kv.length;
        for(var i = 0; i < limit; i += 2)
            ret.put(kv[i], kv[i + 1]);
        return ret;
    }

}
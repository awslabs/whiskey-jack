/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code.cloudformation;

import aws.WhiskeyJack.code.*;
import aws.WhiskeyJack.exl.*;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import static aws.WhiskeyJack.util.Utils.*;
import java.io.*;
import java.lang.reflect.*;
import java.util.*;

@GeneratesCodeFor("*/cloudformation/*")
public class CFBuildController implements DomainGenerationController,
        OverallCodeGenerationDriver.needsOuterBuildController {
    Map<String, Object> resources = new TreeMap<>();
//    String description = "xyzzy";
    OuterBuildController context;
//    private Path rootGenerationDirectory;
    @Override
    public void close() {
        var root = new LinkedHashMap<String, Object>();
//        root.put("Description", deepToString(out.comments));
        root.put("Resources", resources);
        try (var out = new CFTarget(context)){
            out.start(Domain.cloud);
            DataIO.yaml.write(root, out.getWriter());
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
        }
    }
    @Override
    public void generate(DomainCode code) {
        code.getNodes().forEach(n -> {
//            System.out.println("CF generate " + n.getName());
            var resource = new LinkedHashMap<String, Object>();
            resources.put(n.getName(), resource);
            resource.put("Type", n.getStringProp("Type", "AWS::missing::type"));
            var props = n.getMapProp("Properties", null);
            if(props == null) props = Map.of("error",
                        "Properties missing from catalog entry "
                        + n.metadata.getPath());
            resource.put("Properties", dup(n, props));
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
                    for(var i = 0; i<limit; i++) n.add(dup(context, Array.get(o, i)));
                    yield n;
                } else yield o;
            }
        };
    }
    /* evaluate simple path references a.b.c */
    private String eval(Node context, String s) {
        var path = (s.startsWith("$") ? s.substring(1) : s).split("\\.");
        var len = path.length-1;
//        System.out.println("CF path "+join("><", path));
        for(var i = 0; i<len; i++) {
            var next = context.getPort(path[i]);
            if(next.isConnected()) {
                var a = next.getArc(0);
                context = a.otherEnd(next).within;
            } else throw new IllegalArgumentException(path[i]+" not connected, in "+s);
//            System.out.println(path[i]+" "+s+" -> "+context);
        }
        var result = context.getPort(path[len]);
//        System.out.println("CF resolved "+s+" -> "+result);
        if(result==null) throw new IllegalArgumentException(context.getName()+"."+path[len]+" not defined");
        if(result.isConnected() || result.isCode() || result.getValue()==null) throw new IllegalArgumentException(context.getName()+"."+path[len]+" must be a literal constant");
        return result.getValue().toString();
    }
    private OverallCodeGenerationDriver.StrategyPath strategyPath;
    @Override
    public void setStrategyPath(OverallCodeGenerationDriver.StrategyPath p) {
        System.out.println("  cloudfront generator followon path "+p);
        strategyPath = p;
    }
    @Override
    public void setOuterBuildController(OuterBuildController cg) {
        context = cg;
    }
    @Override
    public void prescan() {
    }

}

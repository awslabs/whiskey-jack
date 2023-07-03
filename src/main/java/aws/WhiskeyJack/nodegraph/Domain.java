/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodegraph;

import aws.WhiskeyJack.util.*;
import java.util.*;
import java.util.concurrent.*;

/**
 * Right now this is essentially just an enum, but I've built it as a class
 * because it'll get more flexible and extensible in the future.
 */
public class Domain extends Collectable {
    private final Domain parent;
    private final String name;
    private final String styleName;
    private static int nextSlot = 0;
    private final int slot;
    private Domain(String n, Domain p) {
        slot = p==null ? nextSlot++ : p.slot;
        if(p!=null)
            while(p.parent != null) p = p.parent;
        name = n;
        styleName = name+"Domain";
        parent = p;
    }
    @Override
    public void appendRefTo(StringBuilder sb) {
        sb.append(name);
    }
    @Override
    public Object collect() {
        return parent==null ? name : parent.name+"/"+name;
    }
    public boolean compatibleWith(Domain d) {
        return d == this || d == any || this == any
                 || d == err || this == err;
    }
    private static final Map<String, Domain> domains = new ConcurrentHashMap<>();
    public static final Domain of(String name) {
        var slash = name.lastIndexOf('/');
        return slash<0 ? of(name,null)
                : of(name.substring(slash+1),of(name.substring(0,slash)));
    }
    public static final Domain of(String name, Domain p) {
        return domains.computeIfAbsent(name, n->new Domain(n, p));
    }
    public boolean isA(Domain p) {
        return this==p || parent==p;
    }
    public static Domain[] allInteresting() {
        if(interesting==null)
            interesting = domains.values().stream()
                    .filter(f->f!=any && f!=err && f!=unknown)
                    .toArray(n->new Domain[n]);
        return interesting;
    }
    private static Domain[] interesting;
    public String getStyleName() { return styleName; }
    
    public static final Domain device = of("device");
    public static final Domain gateway = of("gateway");
    public static final Domain cloud = of("cloud");
    public static final Domain browser = of("browser");
    public static final Domain any = of("any");
    public static final Domain err = of("err");
    public static final Domain unknown = of("unknown");
    public String getName() {
        return name;
    }
    @Override
    public String toString() {
        return getName();
    }
}

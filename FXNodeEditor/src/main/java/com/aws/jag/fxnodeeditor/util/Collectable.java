/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.util;

import com.aws.jag.fxnodeeditor.meta.MetaNode;
import java.lang.reflect.*;
import java.util.*;
import java.util.stream.*;

/**
 * Create an object that can be easily be serialized as JSON or YAML
 */
public abstract class Collectable {
    /* I should probably be using Jackson's built-in autoserializer, but I
     * like the control I get by hand-rolling */
    public abstract Object collect();
    public static Object asObject(Object c) {
        var ret = switch(c) {
            case null ->
                null;
            case Collectable o ->
                o.collect();
            case Collection l ->
                l.stream().map(o -> asObject(o)).collect(Collectors.toList());
            case Map m -> {
                var rm = new LinkedHashMap<Object, Object>();
                m.forEach((k, v) -> rm.put(asObject(k), asObject(v)));
                yield rm;
            }
            default -> {
                if(c.getClass().isArray()) {
                    var len = Array.getLength(c);
                    var list = new ArrayList<Object>();
                    for(var i = 0; i < len; i++)
                        list.add(asObject(Array.get(c, i)));
                    yield list;
                }
                yield c;
            }
        };
        if(ret instanceof MetaNode)
            throw new Error(ret.toString());
        return ret;
    }
}

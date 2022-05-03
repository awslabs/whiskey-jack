/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.meta;

import com.nighthacks.fxnodeeditor.util.*;
import static com.nighthacks.fxnodeeditor.util.Utils.*;
import java.util.*;
import java.util.regex.*;

public class Port extends Collectable {
    private final MNode outer;
    public final int slot;
    public final boolean in;
    public String name;
    public String description;
    public String type;
    public Object dflt;
    Port(int s, boolean i, String n, final MNode o) {
        outer = o;
        slot = s;
        in = i;
        name = n;
    }
    @Override
    public Object collect() {
        var m = new LinkedHashMap<String, Object>();
        if(dflt!=null) m.put("default", dflt);
        if(!Objects.equals(type,guessType(dflt)))
            m.put("type", type);
        if(!isEmpty(description))
            m.put("description", description);
        m.put("name",name);
        if(in) m.put("in", true);
        System.out.println(name+" = "+deepToString(m));
        return m.size() == 1  && m.containsKey("default") ? dflt : m;
    }
    @Override
    public String toString() {
        return in ? "▶" + name : name + "▶︎";
    }
    String fullname() {
        return outer.name + (in ? "↓" : "↑") + name;
    }
    public static String guessType(Object s) {
        return switch(s) {
            default ->
                null;
            case Boolean b ->
                "boolean";
            case Float v ->
                "float";
            case Double v ->
                "float";
            case Number n ->
                "int";
            case CharSequence cs -> {
                if(isEmpty(cs))
                    yield null;
                if(isBoolean.reset(cs).matches())
                    yield "boolean";
                if(isInt.reset(cs).matches())
                    yield "int";
                if(isFloat.reset(cs).matches())
                    yield "float";
                yield "String";
            }
        };
    }
    public void setDirty() {
        outer.setDirty();
    }
    // must be used single-threaded
    static final Matcher isBoolean = Pattern.compile("true|false").matcher("");
    static final Matcher isInt = Pattern.compile("[-+]?[0-9]+").matcher("");
    static final Matcher isFloat = Pattern.compile("[-+]?[0-9]*\\.[0-9]*([eE][-+]?[0-9]+)").matcher("");
}

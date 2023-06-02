/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodegraph;

import aws.WhiskeyJack.util.Collectable;
import static aws.WhiskeyJack.util.Utils.*;
import java.util.*;
import java.util.function.*;

public class Type extends Collectable {
    private final String name;
    @SuppressWarnings("")
    private Type(String n) {
        name = n;
        all.put(n, this);
    }
    @SuppressWarnings("unused")
    public static Type guess(Object value) {
        return switch(value) {
            case Number n ->
                number;
            case Boolean b ->
                bool;
            case String s ->
                string;
            case Map m ->
                tuple;
            case null ->
                any;
            default ->
                object;
        };
    }
    @Override
    public void appendRefTo(StringBuilder sb) {
        sb.append(name);
    }
    @Override
    public Object collect() {
        return name;
    }
    public String getName() {
        return name;
    }
    @Override
    public String toString() {
        return getName();
    }
    public static Type of(String name) {
        return isEmpty(name) ? null : all.get(name);
    }
    public static void forEachType(Consumer<Type> f) {
        all.values().forEach(f);
    }
    public static Type[] allInteresting() {
        if(interesting==null)
            interesting = all.values().stream().filter(f->f!=any && f!=err).toArray(n->new Type[n]);
        return interesting;
    }
    public boolean compatibleWith(Type t) {
        return t == this || t == any || this == any
                 || t == err || this == err;
    }

    private static final Map<String, Type> all = new HashMap();
    private static Type[] interesting;
    public static final Type any = new Type("any");
    public static final Type bool = new Type("boolean");
    public static final Type number = new Type("double"); // ?
    public static final Type event = new Type("event");
    public static final Type image = new Type("image"); // ?
    public static final Type string = new Type("string");
    public static final Type tuple = new Type("tuple");
    public static final Type table = new Type("table");
    public static final Type html = new Type("html");
    public static final Type mlmodel = new Type("mlmodel");
    public static final Type code = new Type("code");
    public static final Type object = new Type("object"); // ?

    public static final Type err = new Type("err");
}

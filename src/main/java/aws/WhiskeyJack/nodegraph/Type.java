/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodegraph;

import aws.WhiskeyJack.util.*;
import static aws.WhiskeyJack.util.Utils.*;
import java.util.*;
import java.util.function.*;
import java.util.regex.*;

public class Type extends Collectable {
    private final String name;
    private final Object dflt;
    private boolean error = false;
    @SuppressWarnings("")
    private Type(String n, Object d) {
        name = n;
        dflt = d;
        all.put(n, this);
    }
    public Object coerce(Object v) {
        return coerce(v, null);
    }
    public Object coerce(Object v, Object alternate) {
        return valueCompatibleWith(v) ? v
                : alternate != null && valueCompatibleWith(alternate) ? alternate
                : defaultDefault();
    }
    public static Type guess(Object value) {
        return switch(value) {
            case Number _ ->
                number;
            case Boolean _ ->
                bool;
            case String _ ->
                string;
            case Map _ ->
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
    public Object defaultDefault() {
        return dflt;
    }
    public static Type of(String name, Type dflt) {
        var ret = of(name);
        return ret != null ? ret : dflt;
    }
    public static Type of(String name) {
        return of(name, true);
    }
    public static Type of(String name, boolean create) {
        if(name == null) return null;
        name = name.trim();
        if(name.startsWith("{") && name.endsWith("}")
           || name.startsWith("[") && name.endsWith("]")) {
            name = name.substring(1, name.length() - 1).trim();
            var names = seperators.split(name);
            var cname = String.join(",", names);
            var t = all.get(cname);
            return t != null ? t
                    : new Type(cname, names[0]) {
                {
                    System.out.println("Constructing type " + cname + ": " + deepToString(names));
                }
                @Override
                public boolean isEnum() {
                    return true;
                }
                @Override
                public boolean valueCompatibleWith(Object v) {
                    if(v == null) return false;
                    var vs = v.toString();
                    for(var s: names)
                        if(s.equalsIgnoreCase(vs)) return true;
                    return false;
                }
                @Override
                public String toString() {
                    return "enum" + deepToString(names);
                }
            };
        } else if(isEmpty(name)) return null;
        boolean sic; // https://en.wikipedia.org/wiki/Sic
        if(name.endsWith("(sic)")) {
            sic = true;
            name = name.substring(0, name.length() - 5).trim();
        } else if(name.endsWith("!")) {
            sic = true;
            name = name.substring(0, name.length() - 1).trim();
        } else sic = false;
        var ret = all.get(name);
        if(ret == null && create) {
            ret = new Type(name, null);
            ret.error = true;
        }
        if(sic && ret!=null) ret.error = false;
        return ret;
    }
    public static void forEachType(Consumer<Type> f) {
        all.values().forEach(f);
    }
    public static void forEachErroredType(Consumer<Type> f) {
        all.values().stream().filter(t->t.error).forEach(f);
    }
    public boolean isEnum() {
        return false;
    }
    public boolean isPrimitive() {
        return dflt!=null;
    }
    public boolean valueCompatibleWith(Object v) {
        return true;
    }
    public static Type[] allInteresting() {
        if(interesting == null)
            interesting = all.values().stream().filter(f ->
                    f != any && f != err).toArray(n -> new Type[n]);
        return interesting;
    }
    public boolean compatibleWith(Type t) {
        return t == this || t == any || this == any
               || t == err || this == err;
    }

    private static final Map<String, Type> all = new HashMap();
    private static Type[] interesting;
    public static final Type any = new Type("any", "");
    public static final Type bool = new Type("boolean", false) {
        @Override
        public Boolean coerce(Object v, Object alternate) {
            return Coerce.toBoolean(v);
        }
    };
    public static final Type number = new Type("double", 0) {
        @Override
        public Number coerce(Object v, Object alternate) {
            if(v instanceof Number n) return n;
            var n = Coerce.toDouble(v);
            return !Double.isNaN(n) ? n
                    : alternate instanceof Number a ? a : 0;
        }
    };
    public static final Type event = new Type("event", null);
    public static final Type image = new Type("image", null); // ?
    public static final Type string = new Type("string", "") {
        @Override
        public String coerce(Object v, Object alternate) {
            return v == null ? null : v.toString();
        }
    };
    public static final Type tuple = new Type("tuple", null);
    public static final Type table = new Type("table", null);
    public static final Type database = new Type("database", null);
    public static final Type html = new Type("html", null);
    public static final Type mlmodel = new Type("mlmodel", null);
    public static final Type object = new Type("object", null); // ?    

    public static final Type err = new Type("err", null);
    public static final Type voidType = new Type("void", null);
    public static final Type unknown = new Type("unknown", null);

    private static final Pattern seperators = Pattern.compile(" *[,;] *");
}

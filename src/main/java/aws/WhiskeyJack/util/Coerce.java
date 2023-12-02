/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.util;

import static aws.WhiskeyJack.util.Utils.*;
import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.core.type.*;
import com.fasterxml.jackson.databind.*;
import java.io.*;
import java.lang.reflect.*;
import java.util.*;
import java.util.function.*;
import java.util.regex.*;
import javax.annotation.*;

public final class Coerce {
    private static final Pattern SEPARATORS = Pattern.compile(" *, *");
    private static final Pattern unwrap = Pattern.compile(" *\\[ *(.*) *\\] *");
    public static boolean get(Map m, String key, boolean dflt) {
        var v = m.get(key);
        return v == null ? dflt : Coerce.toBoolean(v);
    }
    public static double get(Map m, String key, double dflt) {
        var v = m.get(key);
        return v == null ? dflt : Coerce.toDouble(v);
    }
    public static String get(Map m, String key, String dflt) {
        var v = m.get(key);
        return v == null ? dflt : Coerce.toString(v);
    }
    public static Map getMap(Map m, String key) {
        var v = m.get(key);
        return v instanceof Map vm ? vm : Map.of();
    }

    private Coerce() {
    }

    /**
     * Convert the object into a boolean value.
     *
     * @param o object
     * @return result.
     */
    public static boolean toBoolean(Object o) {
        if(o instanceof Topic topic)
            o = topic.getOnce();
        return o instanceof Boolean b ? b
                : o instanceof Number n ? n.intValue() != 0
                        : o != null && switch(o.toString()) {
            case "true", "yes", "on", "t", "y", "Y", "1" ->
                true;
            default ->
                false;
        };
    }

    /**
     * Get an object as an integer.
     *
     * @param o object to convert.
     * @return resulting int.
     */
    public static int toInt(Object o) {
        if(o instanceof Topic topic)
            o = topic.getOnce();
        if(o instanceof Boolean b)
            return b ? 1 : 0;
        if(o instanceof Number n)
            return n.intValue();
        if(o != null) try {
            var cs = o instanceof CharSequence ? (CharSequence) o : o.toString();
            return (int) Utils.parseLong(cs);
        } catch(NumberFormatException ignore) {
        }
        return 0;
    }

    /**
     * Convert object to double.
     *
     * @param o object to convert.
     * @return the resulting double value.
     */
    public static double toDouble(Object o) {
        if(o instanceof Topic topic)
            o = topic.getOnce();
        if(o instanceof Boolean boolean1)
            return boolean1 ? 1 : 0;
        if(o instanceof Number number)
            return number.doubleValue();
        if(o != null) try {
            return Double.parseDouble(o.toString().trim());
        } catch(NumberFormatException ignore) {
            return Double.NaN;
        }
        return 0;
    }

    /**
     * Convert object to long.
     *
     * @param o object to convert.
     * @return the resulting long value.
     */
    public static long toLong(Object o) {
        if(o instanceof Topic topic)
            o = topic.getOnce();
        if(o instanceof Boolean boolean1)
            return boolean1 ? 1 : 0;
        if(o instanceof Number number)
            return number.longValue();
        if(o != null) try {
            return Long.parseLong(o.toString());
        } catch(NumberFormatException ignore) {
        }
        return 0;
    }

    /**
     * Convert an object to string or null if it is null.
     *
     * @param o object to convert.
     * @return resulting string.
     */
    @Nullable
    public static String toString(Object o) {
        if(o instanceof Topic topic)
            o = topic.getOnce();
        return o == null ? null : o.toString();
    }

    /**
     * Convert object to an array of strings.
     *
     * @param o object to convert.
     * @return resulting string array.
     */
    public static String[] toStringArray(Object o) {
        if(o instanceof Topic topic)
            o = topic.getOnce();
        if(o == null)
            return emptyStrings;
        if(o instanceof String[] strings)
            return strings;
        if(o.getClass().isArray()) {
            var len = Array.getLength(o);
            var ret = new String[len];
            for(var i = 0; i < len; i++)
                ret[i] = String.valueOf(Array.get(o, i));
            return ret;
        }
        if(o instanceof Collection c)
            return (String[]) c.stream()
                    .map(v -> String.valueOf(v))
                    .toArray(n -> new String[n]);
        var body = o.toString();
        var uw = unwrap.matcher(body);
        if(uw.matches())
            body = uw.group(1);
        body = body.trim();
        if(isEmpty(body))
            return emptyStrings;
        return SEPARATORS.split(body);
    }
    private static final String[] emptyStrings = new String[0];

    /**
     * Convert object to a list of strings.
     *
     * @param o object to convert.
     * @return resulting list.
     */
    public static List<String> toStringList(Object o) {
        return Arrays.asList(toStringArray(o));
    }

    public static <T extends Enum<?>> T toEnum(Class<T> cl, Object o) {
        return toEnum(cl, o, null);
    }

    /**
     * Convert an object to an enum of class clazz with a default value of dflt.
     *
     * @param clazz enum class to convert into.
     * @param o object to be converted.
     * @param dflt default value if the conversion fails.
     * @param <T> Enum type to coerce to.
     * @return enum value or default.
     */
    public static <T extends Enum<?>> T toEnum(Class<T> clazz, Object o, T dflt) {
        if(o == null)
            return dflt;
        if(o instanceof Topic topic) {
            o = topic.getOnce();
            if(o == null)
                return dflt;
        }
        if(clazz.isAssignableFrom(o.getClass()))
            return (T) o;
        var values = clazz.getEnumConstants();
        if(o instanceof Number number)
            return values[Math.max(0, Math.min(values.length - 1, number.intValue()))];
        var s = Coerce.toString(o);
        if(s != null) {
            var l = s.length();
            for(var v: values) {
                var vs = v.toString();
                if(vs.length() < l)
                    continue;
                if(vs.regionMatches(true, 0, s, 0, l))
                    return v;
            }
        }
        return dflt;
    }

    public static Collection<Object> toCollection(Object o) {
        if(o == null) return Collections.emptyList();
        if(o.getClass().isArray()) {
            var sz = Array.getLength(o);
            if(sz == 0) return Collections.emptyList();
            var ret = new ArrayList<Object>(sz);
            for(var i = 0; i < sz; i++)
                ret.add(Array.get(o, i));
            return ret;
        }
        if(o instanceof Collection c)
            return c;
        return Collections.singletonList(o);
    }
}

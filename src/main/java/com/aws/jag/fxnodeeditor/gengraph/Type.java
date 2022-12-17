/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

import java.util.*;
import java.util.function.*;
import java.util.stream.*;

public class Type {
    private final String name;
    @SuppressWarnings("")
    public Type(String n) {
        name = n;
        all.put(n, this);
    }
    public String getName() {
        return name;
    }
    @Override
    public String toString() {
        return "Type<" + getName() + ">";
    }
    public static Type of(String name) {
        return all.get(name);
    }
    public static void forEachType(Consumer<Type> f) {
        all.values().forEach(f);
    }
    public static Collection<String> allTypes() {
        return all.values().stream()
                .map(Type::getName)
                .collect(Collectors.toList());
    }

    private static final Map<String, Type> all = new HashMap();
    public static final Type bool_t = new Type("boolean");
    public static final Type double_t = new Type("double");
    public static final Type image_t = new Type("image");
    public static final Type int_t = new Type("integer");
    public static final Type json_t = new Type("json");
    public static final Type object_t = new Type("object");

    public static final Type err_t = new Type("err");
}

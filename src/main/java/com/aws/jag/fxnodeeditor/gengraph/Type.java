/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

public class Type {
    final String name;
    public Type(String n) {
        name = n;
    }
    public String getName() {
        return name;
    }
    @Override
    public String toString() {
        return "Type<" + getName() + ">";
    }
    public static final Type int_t = new Type("integer");
    public static final Type double_t = new Type("double");
    public static final Type bool_t = new Type("boolean");
    public static final Type image_t = new Type("image");
    public static final Type json_t = new Type("json");
}

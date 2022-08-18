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
}

/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

/**
 * Right now this is essentially just an enum, but I've built it as a class
 * because it'll get more flexible and extensible in the future.
 */
public class Domain {
    private final String name;
    Domain(String n) {
        name = n;
    }
    public boolean compatibleWith(Domain d) {
        return d == this || d==any || this==any;
    }
    public static final Domain device = new Domain("device");
    public static final Domain gateway = new Domain("gateway");
    public static final Domain cloud = new Domain("cloud");
    public static final Domain any = new Domain("any");
    @Override
    public String toString() {
        return name;
    }
}

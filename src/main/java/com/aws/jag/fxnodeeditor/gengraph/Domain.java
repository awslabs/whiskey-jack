/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

import java.util.*;
import java.util.concurrent.*;

/**
 * Right now this is essentially just an enum, but I've built it as a class
 * because it'll get more flexible and extensible in the future.
 */
public class Domain {
    private final String name;
    @SuppressWarnings("LeakingThisInConstructor")
    private Domain(String n) {
        name = n;
    }
    public boolean compatibleWith(Domain d) {
        return d == this || d == any || this == any;
    }
    private static final Map<String, Domain> domains = new ConcurrentHashMap<>();
    public static final Domain of(String name) {
        return domains.computeIfAbsent(name, n->new Domain(n));
    }
    public static final Domain device = of("device");
    public static final Domain gateway = of("gateway");
    public static final Domain cloud = of("cloud");
    public static final Domain any = of("any");
    @Override
    public String toString() {
        return name;
    }
}

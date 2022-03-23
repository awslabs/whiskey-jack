/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import org.junit.jupiter.api.*;

public class CheapSetTest {
    @Test
    public void testSomeMethod() {
        T();
        T("a");
        T("a", "b");
        T("a", "b", "c");
        T("a", "b", "c", "d");
    }
    private void T(String... s) {
        CheapSet<String> c = CheapSet.ofAll(s);
        System.out.println(s.length+": "+c);
        Assertions.assertEquals(s.length, c.size());
        for(String v: s)
            Assertions.assertTrue(c.contains(v));
    }

}

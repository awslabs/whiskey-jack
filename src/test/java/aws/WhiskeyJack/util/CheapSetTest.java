/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.util;

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
        var c = CheapSet.ofAll(s);
//        System.out.println(s.length+": "+c);
        Assertions.assertEquals(s.length, c.size());
        for(var v: s)
            Assertions.assertTrue(c.contains(v));
    }

}

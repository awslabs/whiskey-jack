/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;

public class GlobTest {
    @Test
    public void t1() {
        var p = Glob.compile("*.jpg");
        assertEquals("", p.matches("foo.jpg"));
        assertEquals("", p.matches(".jpg"));
        assertEquals(null, p.matches("foo.jpeg"));
    }
    @Test
    public void t2(){
        var p = Glob.compile("/*");
        assertEquals("/a/foo.jpg", p.matches("/a/foo.jpg"));
        assertEquals(null, p.matches("a/foo.jpg"));
    }
    @Test
    public void t3(){
        var p = Glob.compile("/*.jpg");
        assertEquals("/a/foo", p.matches("/a/foo.jpg"));
        assertEquals(null, p.matches("a/foo.jpg"));
        assertEquals(null, p.matches("/a/foo.jpeg"));
//        var t = new Glob();
    }
    @Test
    public void t4() {
        var p = Glob.compile("*/cl/*");
        assertEquals("/lambda", p.matches("/cloud/cl/lambda"));
        assertEquals(null, p.matches("/cloud/clx/lambda"));
        assertEquals(null, p.matches("/cloud/dcl/lambda"));
    }
}

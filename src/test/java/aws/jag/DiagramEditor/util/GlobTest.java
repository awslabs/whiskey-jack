/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.util;

import aws.jag.DiagramEditor.code.Glob;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class GlobTest {
    @Test
    public void t1() {
        var p = Glob.compile("*.jpg");
        assertTrue(p.matches("foo.jpg"));
        assertTrue(p.matches(".jpg"));
        assertFalse(p.matches("foo.jpeg"));
    }
    @Test
    public void t2(){
        var p = Glob.compile("/*");
        assertTrue(p.matches("/a/foo.jpg"));
        assertFalse(p.matches("a/foo.jpg"));
    }
    @Test
    public void t3(){
        var p = Glob.compile("/*.jpg");
        assertTrue(p.matches("/a/foo.jpg"));
        assertFalse(p.matches("a/foo.jpg"));
        assertFalse(p.matches("/a/foo.jpeg"));
//        var t = new Glob();
    }
    
}

/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.exl;

import java.io.*;
import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.*;

public class TokenizerTest {

    public TokenizerTest() {
    }

//    @Test
    public void t1() throws IOException {
        System.out.println(Tokenizer.NULL);
        assertTrue(Tokenizer.NULL.isKeyword());
        Tokenizer tz = new Tokenizer(new StringReader("hello '\\\"world\\\"' 42 < > <= ≤ >= ≥ != ≠ == ≡ "));
        Token t;
        while((t = tz.get()) != Tokenizer.EOF)
            System.out.println(t);
    }

    @Test
    public void t2() throws IOException {
        Tokenizer tz = new Tokenizer(new StringReader(
                "hello '\\\"world\\\"' 42 43.0 42.1 < > <= ≤ >= ≥ != ≠ == ≡ ! ¬ null ∅ "));
        assertEquals("0:hello", tz.get().toString());
        assertEquals("1:\"world\"", tz.get().toString());
        assertEquals("2:42", tz.get().toString());
        assertEquals("2:43", tz.get().toString());
        assertEquals("2:42.1", tz.get().toString());
        assertEquals(tz.LT, tz.get());
        assertEquals(tz.GT, tz.get());
        assertEquals(tz.LE, tz.get());
        assertEquals(tz.LE, tz.get());
        assertEquals(tz.GE, tz.get());
        assertEquals(tz.GE, tz.get());
        assertEquals(tz.NE, tz.get());
        assertEquals(tz.NE, tz.get());
        assertEquals(tz.EQ, tz.get());
        assertEquals(tz.EQ, tz.get());
        assertEquals(tz.NOT, tz.get());
        assertEquals(tz.NOT, tz.get());
        assertEquals(tz.NULL, tz.get());
        assertEquals(tz.NULL, tz.get());
    }

    @Test
    public void t3() throws IOException {
        Tokenizer tz = new Tokenizer(new StringReader(
                "hello '\\\"world\\\"' 42 43.0 42.1 < > <= ≤ >= ≥ != ≠ == ≡ ! ¬ null ∅ "));
        assertEquals("0:hello", tz.peek(0).toString());
        assertEquals("1:\"world\"", tz.peek(1).toString());
        assertEquals("2:42", tz.peek(2).toString());
//        assertEquals("2:43", tz.get().toString());
//        assertEquals("2:42.1", tz.get().toString());
        assertEquals(tz.LT, tz.peek(5));
//        assertEquals(tz.GT, tz.get());
//        assertEquals(tz.LE, tz.get());
//        assertEquals(tz.LE, tz.get());
//        assertEquals(tz.GE, tz.get());
//        assertEquals(tz.GE, tz.get());
//        assertEquals(tz.NE, tz.get());
//        assertEquals(tz.NE, tz.get());
//        assertEquals(tz.EQ, tz.get());
//        assertEquals(tz.EQ, tz.get());
//        assertEquals(tz.NOT, tz.get());
//        assertEquals(tz.NOT, tz.get());
//        assertEquals(tz.NULL, tz.get());
//        assertEquals(tz.NULL, tz.get());
        assertEquals("0:hello", tz.get().toString());
        assertEquals("1:\"world\"", tz.get().toString());
        assertEquals("2:42", tz.get().toString());
        assertEquals("2:43", tz.get().toString());
        assertEquals("2:42.1", tz.get().toString());
        assertEquals(tz.LT, tz.get());
        assertEquals(tz.GT, tz.get());
        assertEquals(tz.LE, tz.get());
        assertEquals(tz.LE, tz.get());
        assertEquals(tz.GE, tz.get());
        assertEquals(tz.GE, tz.get());
        assertEquals(tz.NE, tz.get());
        assertEquals(tz.NE, tz.get());
        assertEquals(tz.EQ, tz.get());
        assertEquals(tz.EQ, tz.get());
        assertEquals(tz.NOT, tz.get());
        assertEquals(tz.NOT, tz.get());
        assertEquals(tz.NULL, tz.get());
        assertEquals(tz.NULL, tz.get());
    }

}

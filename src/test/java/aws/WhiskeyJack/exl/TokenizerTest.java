/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.exl;

import java.io.*;
import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.*;

public class TokenizerTest {

    public TokenizerTest() {
    }

//    @Test
    public void t1() throws IOException {
        System.out.println(Vocabulary.NULL);
        assertTrue(Vocabulary.NULL.isKeyword());
        var tz = new Tokenizer(new StringReader("hello '\\\"world\\\"' 42 < > <= ≤ >= ≥ != ≠ == ≡ "));
        Token t;
        while((t = tz.get()) != Vocabulary.EOF)
            System.out.println(t);
    }

    @Test
    public void t2() throws IOException {
        var tz = new Tokenizer(new StringReader(
                "hello '\\\"world\\\"' 42 43.0 42.1 < > <= ≤ >= ≥ != ≠ == ≡ ! ¬ null ∅ "));
        assertEquals("〖hello〗", tz.get().toString());
        assertEquals("〖\"world\"〗", tz.get().toString());
        assertEquals("〖42〗", tz.get().toString());
        assertEquals("〖43〗", tz.get().toString());
        assertEquals("〖42.1〗", tz.get().toString());
        assertEquals(Vocabulary.LT, tz.get());
        assertEquals(Vocabulary.GT, tz.get());
        assertEquals(Vocabulary.LE, tz.get());
        assertEquals(Vocabulary.LE, tz.get());
        assertEquals(Vocabulary.GE, tz.get());
        assertEquals(Vocabulary.GE, tz.get());
        assertEquals(Vocabulary.NE, tz.get());
        assertEquals(Vocabulary.NE, tz.get());
        assertEquals(Vocabulary.EQ, tz.get());
        assertEquals(Vocabulary.EQ, tz.get());
        assertEquals(Vocabulary.NOT, tz.get());
        assertEquals(Vocabulary.NOT, tz.get());
        assertEquals(Vocabulary.NULL, tz.get());
        assertEquals(Vocabulary.NULL, tz.get());
    }

    @Test
    public void t3() throws IOException {
        var tz = new Tokenizer(new StringReader(
                "hello '\\\"world\\\"' 42 43.0 42.1 < > <= ≤ >= ≥ != ≠ == ≡ ! ¬ null ∅ "));
        assertEquals("〖hello〗", tz.peek(0).toString());
        assertEquals("〖\"world\"〗", tz.peek(1).toString());
        assertEquals("〖42〗", tz.peek(2).toString());
//        assertEquals("2:43", tz.get().toString());
//        assertEquals("2:42.1", tz.get().toString());
        assertEquals(Vocabulary.LT, tz.peek(5));
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
        assertEquals("〖hello〗", tz.get().toString());
        assertEquals("〖\"world\"〗", tz.get().toString());
        assertEquals("〖42〗", tz.get().toString());
        assertEquals("〖43〗", tz.get().toString());
        assertEquals("〖42.1〗", tz.get().toString());
        assertEquals(Vocabulary.LT, tz.get());
        assertEquals(Vocabulary.GT, tz.get());
        assertEquals(Vocabulary.LE, tz.get());
        assertEquals(Vocabulary.LE, tz.get());
        assertEquals(Vocabulary.GE, tz.get());
        assertEquals(Vocabulary.GE, tz.get());
        assertEquals(Vocabulary.NE, tz.get());
        assertEquals(Vocabulary.NE, tz.get());
        assertEquals(Vocabulary.EQ, tz.get());
        assertEquals(Vocabulary.EQ, tz.get());
        assertEquals(Vocabulary.NOT, tz.get());
        assertEquals(Vocabulary.NOT, tz.get());
        assertEquals(Vocabulary.NULL, tz.get());
        assertEquals(Vocabulary.NULL, tz.get());
    }

}

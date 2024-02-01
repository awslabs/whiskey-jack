/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.exl;

import aws.WhiskeyJack.util.*;
import java.io.*;
import java.util.*;

public final class Token {
    public static final int identifierKind = 0;
    public static final int stringKind = 1;
    public static final int numberKind = 2;
    private static final int firstToken = 3;
    private static int kindSequence = firstToken;
    private final Object body;
    private final int kind;
    private Token(Object b, int k) {
        body = b;
        assert b!=null;
        kind = k;
    }
    @Override
    public String toString() {
        return "〖" + body + "〗";
    }
    public StringBuilder appendTo(StringBuilder sb) {
        if(sb == null) sb = new StringBuilder();
        if(body != null)
            if(kind == stringKind) try {
                sb.append('"');
                Utils.deepToStringQuoted(body, sb, 99999);
                sb.append('"');
            } catch(IOException ex) {
            } else sb.append(body);
        return sb;
    }
    private static Token of(String s, int k) {
        return cache.computeIfAbsent(s, S -> new Token(s, k));
    }
    public static Token string(String s) {
        return new Token(s, stringKind);
    }
    public static Token identifier(String s) {
        return of(s, identifierKind);
    }
    public static Token keyword(String s) {
        return of(s, kindSequence++);
    }
    public static Token number(Number n) {
        return cache.computeIfAbsent(n, N -> new Token(n, numberKind));
    }
    private static final Map<Object, Token> cache = new HashMap<>();
    public String getBody() {
        return String.valueOf(body);
    }
    public int getKind() {
        return kind;
    }
    public Number getNum() {
        return (Number)body;
    }
    @Override
    public int hashCode() {
        return body.hashCode();
    }
    @Override
    public boolean equals(Object t) {
        return t == this ? true
            : t instanceof Token T ? kind==T.kind && body.equals(T.body)
            : false;
    }
    public boolean isIdentifier() {
        return kind == identifierKind;
    }
    public boolean isString() {
        return kind == stringKind;
    }
    public boolean isNumber() {
        return kind == numberKind;
    }
    public boolean isKeyword() {
        return kind >= firstToken;
    }
    public static int kindTableSize() {
        return kindSequence;
    }
}

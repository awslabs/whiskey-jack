/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.exl;

import aws.WhiskeyJack.util.*;
import java.io.*;
import java.util.*;

public class Token {
    public static final int identifierType = 0;
    public static final int stringType = 1;
    public static final int numberType = 2;
    private static int typeSequence = numberType;
    private final String body;
    private final int type;
    private final Number num;
    private Token(String b, int t, Number n) {
        body = b;
        type = t;
        num = n;
    }
    @Override
    public String toString() {
        return "〖" + (getBody() != null ? getBody() : getNum()) + "〗";
    }
    public StringBuilder appendTo(StringBuilder sb) {
        if(sb == null) sb = new StringBuilder();
        if(body != null)
            if(type == stringType) try {
                sb.append('"');
                Utils.deepToStringQuoted(body, sb, 99999);
                sb.append('"');
            } catch(IOException ex) {
            } else sb.append(body);
        else sb.append(num);
        return sb;
    }
    private static Token of(String s, int t) {
        return cache.computeIfAbsent(s, S -> new Token(S, t, null));
    }
    public static Token string(String s) {
        return new Token(s, stringType, null);
    }
    public static Token identifier(String s) {
        return of(s, identifierType);
    }
    public static Token keyword(String s) {
        return of(s, ++typeSequence);
    }
    public static Token number(Number n) {
        return new Token(null, numberType, n);
    }
    private static final Map<String, Token> cache = new HashMap<>();
    public String getBody() {
        return body;
    }
    public int getType() {
        return type;
    }
    public Number getNum() {
        return num;
    }
    @Override
    public int hashCode() {
        return switch(type) {
            case identifierType ->
                body.hashCode();
            case stringType ->
                body.hashCode();
            case numberType ->
                num.hashCode();
            default ->
                type;
        };
    }
    @Override
    public boolean equals(Object t) {
        return t == this
                ? true
                : t instanceof Token T
                        ? switch(type) {
            case stringType ->
                body.equals(T.body);
            case numberType ->
                num.equals(T.num);
            default ->
                false;
        }
                : false;
    }
    public boolean isIdentifier() {
        return type == identifierType;
    }
    public boolean isString() {
        return type == stringType;
    }
    public boolean isNumber() {
        return type == numberType;
    }
    public boolean isKeyword() {
        return type > numberType;
    }
    public static int typeTableSize() {
        return typeSequence + 1;
    }
}

/*
 * SPDX-FileCopyrightText: Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.exl;

import java.io.*;
import java.util.*;

public class Tokenizer {
    private final Reader in;
    private char nextc;
    private static final char EOTchar = (char) 4;  // see asciitable.com
    int line = 1, col;
    private final StringBuilder sb = new StringBuilder();
    private Token[] lab = new Token[2];  // lab==Look Ahead Buffer
    private int labPosition, labLimit;
    public Tokenizer(Reader in0) {
        in = in0 instanceof BufferedReader inb ? inb : new BufferedReader(in0);
    }
    public Tokenizer(String s) {
        in = new StringReader(s);
    }
    private char getc() throws IOException {
        var ret = nextc;
        if(ret == 0) {
            var ic = in.read();
            if(ic < 0)
                return nextc = EOTchar;
            ret = (char) ic;
            if(ret == '\n') {// TODO: someday, care about \r
                line++;
                col = 0;
            }
            col++;
        } else nextc = 0;
        return ret;
    }
    private char peekc() throws IOException {
        return nextc = getc();
    }
    private void pushbackc(char c) {
        nextc = c;
    }
    /**
     * Get the next token
     */
    public Token get() throws IOException {
        var t = labPosition >= labLimit ? actuallyGetToken() : lab[labPosition++];
        return t;
    }
    /**
     * * peek ahead <i>n</i> tokens.peek(0) returns the same result at get()
     */
    public Token peek(int n) throws IOException {
        if(labPosition >= labLimit)
            labPosition = labLimit = 0;
        while(labLimit - labPosition <= n) {
            if(labLimit >= lab.length)
                lab = Arrays.copyOf(lab, lab.length * 2);
            lab[labLimit++] = actuallyGetToken();
        }
        return lab[labPosition + n];
    }
    public Token actuallyGetToken() throws IOException {
        char c;
        while((c = getc()) <= ' ')
            if(c == EOTchar) return EOF;
        if(Character.isJavaIdentifierStart(c)) {
            sb.setLength(0);
            do {
                sb.append(c);
                c = getc();
            } while(Character.isJavaIdentifierPart(c) && c!=EOTchar);
            pushbackc(c);
            return Token.identifier(sb.toString());
        }
        if(Character.isDigit(c) || c == '.' && Character.isDigit(peekc())) {
            sb.setLength(0);
            var seendot = c == '.';
            do {
                sb.append(c);
                c = getc();
                if(c == '.')
                    if(seendot) break;
                    else seendot = true;
            } while(Character.isDigit(c) || c == '.');
            pushbackc(c);
            // TODO handle E+n exponent notation
            // TODO handle 0x..., nnL, ...
            try {
                var parsed = Double.parseDouble(sb.toString());
                var asInt = (int) parsed;
                return Token.number(asInt == parsed ? (Number) asInt : (Number) parsed);
            } catch(NumberFormatException nfe) {
                syntaxError("Can't parse " + sb + " (" + nfe.getMessage() + '"');
            }
        }
        if(c == '"' || c == '\'') {
            sb.setLength(0);
            // TODO: handle test blocks https://docs.oracle.com/javase/specs/jls/se17/html/jls-3.html#jls-3.10.6
            var quote = c;
            while((c = getc()) != quote) {
                if(c == '\n')
                    syntaxError("Unterminated String \"" + sb);
                if(c == '\\') {
                    c = getc();
                    if(c == '\n') continue;  // absorb \<newline>
                    switch(c) {
                        case 'n' -> c = '\n';
                        case 'r' -> c = '\r';
                        case 't' -> c = '\t';
                        case 'b' -> c = '\b';
                        case 'f' -> c = '\f';
                        case 's' -> c = ' ';
                    }
                }
                sb.append(c);
            }
            return Token.string(sb.toString());
        }
        // not ident, string or number
        var t = tokenRoot;
        while(true) {
            var tn = t.findChild(c);
            if(tn == null) break;
            t = tn;
            c = getc();
        }
        pushbackc(c);
        var ret = t.token;
        if(ret == null) syntaxError("Unknown symbol: '" + c + "'");
        if(ret==SlashSlashCOMMENT) {
            while((c = getc())!='\n' && c!=EOTchar) {}
            return actuallyGetToken();
        }
        if(ret==SlashStarCOMMENT) {
            var prev = ' ';
            while((c = getc())!=EOTchar) {
                if(c=='/' && prev == '*') break;
                prev = c;
            }
            return actuallyGetToken();
        }
        return ret;
    }
    public void syntaxError(String error) throws SyntaxError {
        throw new SyntaxError(error, this);
    }

    private static Trie tokenRoot = new Trie((char) 0, null);

    private static class Trie {
        Trie(char C, Trie s) {
            c = C;
            sibling = s;
        }
        final char c;
        Token token;
        Trie sibling, child;
        Trie parent;
        Trie findChild(char c) {
            var st = child;
            while(st != null && st.c != c)
                st = st.sibling;
            return st;
        }
        void setToken(Token T) {
            if(token != null)
                System.out.println("Token overwrite " + this + " " + T);
            token = T;
        }
        @Override
        public String toString() {
            return (parent == null ? "" : parent.toString()) + c;
        }
    }
    static private Trie define(String s) {
        var six = 0;
        var len = s.length();
        var t = tokenRoot;
        while(six < len) {
            var c = s.charAt(six);
            var st = t.findChild(c);
            if(st == null)
                t.child = st = new Trie(c, t.child);
            t = st;
            six++;
        }
        return t;
    }
    public static Token operator(String... ops) {
        var token = Token.keyword(ops[0]);
        for(var op: ops)
            define(op).setToken(token);
        return token;
    }
    public static Token keyword(String key, String... ops) {
        var token = Token.keyword(key);
        for(var op: ops)
            define(op).setToken(token);
        return token;
    }
    public static final Token EOF = Token.keyword("-EOF-");
    public static final Token UNKNOWN = operator(" ¡unknown!");
    public static final Token HASPROPERTIES = operator(" ¡hasproperties!");
    public static final Token LPAREN = operator("(");
    public static final Token RPAREN = operator(")");
    public static final Token LSQUARE = operator("[");
    public static final Token RSQUARE = operator("]");
    public static final Token LBRACE = operator("{");
    public static final Token RBRACE = operator("}");
    public static final Token LT = operator("<");
    public static final Token LE = operator("<=", "≤");
    public static final Token GT = operator(">");
    public static final Token GE = operator(">=", "≥");
    public static final Token EQ = operator("==", "≡");
    public static final Token NE = operator("!=", "≠");
    public static final Token NOT = operator("!", "¬");
    public static final Token DOT = operator(".");
    public static final Token COLON = operator(":");
    public static final Token COMMA = operator(",");
    public static final Token SEMI = operator(";");
    public static final Token DECLAREASSIGN = operator(":=");
    public static final Token DECLAREASSIGNFINAL = operator(":==");
    public static final Token ASSIGN = operator("=", "←");
    public static final Token ANDAND = operator("&&", "∧");
    public static final Token AND = operator("&");
    public static final Token PLUS = operator("+");
    public static final Token MINUS = operator("-", "−"); // minus sign \u2212
    public static final Token OR = operator("|");
    public static final Token OROR = operator("||", "∨");
    public static final Token DIVIDE = operator("/", "÷");
    public static final Token MULTIPLY = operator("*", "×");
    public static final Token QUESTION = operator("?", "→", "⇒");
    public static final Token SlashSlashCOMMENT = operator("//");
    public static final Token SlashStarCOMMENT = operator("/*");
    public static final Token NULL = keyword("null", "∅"); // empty set \u2205
    public static final Token RETURN = keyword("return", "∴"); // therefore \u2234˙
    public static final Token IF = keyword("if");
    public static final Token ELSE = keyword("else");
    public static final Token FOR = keyword("for", "∀"); // for all \u2200
    public static final Token ELEMENTOF = operator("∈"); // element of \u2208
    public static final Token DO = keyword("do");
    public static final Token WHILE = keyword("while");
    public static final Token SWITCH = keyword("switch");
    public static final Token CASE = keyword("case");
    public static final Token DEFAULT = keyword("default");
    
    
    public static int typeTableSize() {
        return Token.typeTableSize();
    }
}

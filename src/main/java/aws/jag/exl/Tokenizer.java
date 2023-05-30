/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
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
            if(c == EOTchar) return Vocabulary.EOF;
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
        if(ret==Vocabulary.SlashSlashCOMMENT) {
            while((c = getc())!='\n' && c!=EOTchar) {}
            return actuallyGetToken();
        }
        if(ret==Vocabulary.SlashStarCOMMENT) {
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

    static class Trie {
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
    static Trie define(String s) {
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
}

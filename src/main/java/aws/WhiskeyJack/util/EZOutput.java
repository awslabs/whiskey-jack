/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.util;

import java.io.*;
import java.nio.file.*;

/**
 * Easy output.  A StringTemplate with an attached output destination.
 * It avoids the need for println &friends, and it cuts down on the
 * number of String copies.  Sample usages:
 * <pre>
 * O."hello \{name\}";  // prints to stdout
 * D."I saw \{exception\}";  // Same as O, except that it does nothing if the
 *                           // SILENTDEBUG environment variable is set.
 * </pre>
 * The interpolated values are printed much like the STR StringTemplate, except
 * that Throwables, Maps, Collections and Arrays are expanded.
 */
public class EZOutput implements StringTemplate.Processor<Void, RuntimeException>, Closeable {
    public static final EZOutput O = of(System.out);
    public static final EZOutput E = of(System.err);
    public static final EZOutput devnull = new EZOutput(null) {
                @Override
                public Void process(StringTemplate t) {
                    return null; // output goes nowhere
                }
            };

    public static final EZOutput D = System.getenv("SILENTDBG") != null ? devnull : O;
    public static EZOutput of(Writer w) { return new EZOutput(w); }
    public static EZOutput of(OutputStream w) { return of(new OutputStreamWriter(w)); }
    public static EZOutput of(Path w) throws IOException {
        try {
            return of(Files.newBufferedWriter(w));
        } catch(FileNotFoundException ex) {
            Files.createDirectories(w.getParent());
            return of(Files.newBufferedWriter(w));
        }
    }
    private final Writer out;
    private EZOutput(Writer o) {
        out = o;
    }
    @Override
    public void close() {
        Utils.close(out);
    }
    @Override
    @SuppressWarnings({"UseSpecificCatch", "ThrowableResultIgnored"})
    public Void process(StringTemplate t) throws RuntimeException {
        var vals = t.values().iterator();
        try {
            for(String s: t.fragments()) {
                out.write(s);
                if(vals.hasNext()) {
                    var val = vals.next();
                    if(val instanceof Throwable err) {
                        while(err.getCause() != null) err = err.getCause();
                        out.append(err.toString());
                        int limit = 10;
                        for(var ln: err.getStackTrace()) {
                            out.append("\n\t")
                                .append(ln.getMethodName())
                                .append('@')
                                .append(ln.getFileName())
                                .append(':');
                            Utils.appendLong(ln.getLineNumber(), out);
                            if(--limit <= 0) break;
                        }
                    } else
                        Utils.deepToString(val, out, 200);
                }
            }
            out.append('\n');
            out.flush();
        } catch(Throwable ioe) {
            ioe.printStackTrace(System.out);
        }
        Utils.flush(out);
        return null;
    }
}

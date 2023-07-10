/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.util;

import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.module.*;
import com.fasterxml.jackson.databind.ser.std.*;
import com.fasterxml.jackson.dataformat.yaml.*;
import java.io.*;
import java.net.*;
import java.nio.charset.*;
import java.nio.file.*;

public class YAMLio {
    static private final ObjectMapper fileio = new ObjectMapper(
            new YAMLFactory()
                    .enable(YAMLGenerator.Feature.MINIMIZE_QUOTES)
                    .enable(YAMLGenerator.Feature.USE_PLATFORM_LINE_BREAKS)
                    .enable(JsonParser.Feature.ALLOW_COMMENTS)
                    .enable(JsonParser.Feature.ALLOW_SINGLE_QUOTES)
                    .enable(JsonParser.Feature.ALLOW_YAML_COMMENTS)
                    .enable(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES)
    ).configure(JsonGenerator.Feature.AUTO_CLOSE_TARGET, false);
    public static void write(Object what, Writer where) throws IOException {
        fileio.writeValue(where, what);
    }
    public static boolean write(Object what, Path file) {
        if(file == null)
            return true;
        try(var w = CommitableWriter.abandonOnClose(file)) {
            System.out.println("Writing " + file);
            fileio.writeValue(w, what);
            w.commit();
            return true;
        } catch(IOException ioe) {
            Utils.getUltimateCause(ioe).printStackTrace(System.out);
            return false;
        }
    }
    public static String asString(Object what) {
        StringWriter out = new StringWriter();
        try {
            write(what,out);
        } catch(IOException ex) {}
        return out.toString();
    }
    public static Object read(Reader in0) {
        // always closes in0
        if(in0 == null) return true;
        try(var in = in0) {
            return fileio.readValue(in, Object.class);
        } catch(IOException ioe) {
            ioe.printStackTrace(System.out);
            return false;
        }
    }
    public static Object read(InputStream in) {
        return read(new BufferedReader(
                new InputStreamReader(in, StandardCharsets.UTF_8)));
    }
    public static Object read(URL in) {
        if(in==null) return null;
        try {
            return read(in.openStream());
        } catch(IOException ex) {
            return false;
        }
    }
    public static Object read(Path in) {
        if(in==null) return null;
        try {
            return read(Files.newBufferedReader(in, StandardCharsets.UTF_8));
        } catch(IOException ex) {
            return false;
        }
    }
    public static void addSerializer(Class c, StdSerializer s) {
        var module = new SimpleModule();
        module.addSerializer(c, s);
        fileio.registerModule(module);
    }
}

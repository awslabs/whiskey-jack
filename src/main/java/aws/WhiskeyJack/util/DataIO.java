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

public class DataIO {

    static public final DataIO yaml = new DataIO(".yaml", new ObjectMapper(
            new YAMLFactory()
                    .enable(YAMLGenerator.Feature.MINIMIZE_QUOTES)
                    .enable(YAMLGenerator.Feature.USE_PLATFORM_LINE_BREAKS)
                    .enable(JsonParser.Feature.ALLOW_COMMENTS)
                    .enable(JsonParser.Feature.ALLOW_SINGLE_QUOTES)
                    .enable(JsonParser.Feature.ALLOW_YAML_COMMENTS)
                    .enable(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES)
    ).configure(JsonGenerator.Feature.AUTO_CLOSE_TARGET, false));
    static public final DataIO json = new DataIO(".json", new ObjectMapper(
            new JsonFactory()
                    .enable(JsonParser.Feature.ALLOW_COMMENTS)
                    .enable(JsonParser.Feature.ALLOW_SINGLE_QUOTES)
                    .enable(JsonParser.Feature.ALLOW_YAML_COMMENTS)
                    .enable(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES)
    ).configure(JsonGenerator.Feature.AUTO_CLOSE_TARGET, false));

    private DataIO(String e, ObjectMapper om) {
        extension = e;
        fileio = om;
    }
    private final ObjectMapper fileio;
    private final String extension;
    public void write(Object what, Writer where) throws IOException {
        fileio.writeValue(where, what);
    }
    public boolean write(Object what, Path file) {
        if(file == null)
            return true;
        try(var w = CommitableWriter.abandonOnClose(file)) {
            System.out.println("Writing " + file);
            fileio.writeValue(w, what);
            w.commit();
            return true;
        } catch(IOException ioe) {
            ioe.printStackTrace(System.out);
            return false;
        }
    }
    public String asString(Object what) {
        StringWriter out = new StringWriter();
        try {
            write(what, out);
        } catch(IOException ex) {
        }
        return out.toString();
    }
    public Object read(Reader in0) {
        // always closes in0
        if(in0 == null)
            return true;
        try(var in = new tabless(in0)) {
            return fileio.readValue(in, Object.class);
        } catch(IOException ioe) {
            ioe.printStackTrace(System.out);
            return false;
        }
    }
    public Object read(InputStream in) {
        return read(new BufferedReader(
                new InputStreamReader(in, StandardCharsets.UTF_8)));
    }
    public Object read(URL in) {
        if(in == null)
            return null;
        try {
            return read(in.openStream());
        } catch(IOException ex) {
            return false;
        }
    }
    public Object read(Path in) {
        if(in == null)
            return null;
        try {
            return read(Files.newBufferedReader(in, StandardCharsets.UTF_8));
        } catch(IOException ex) {
            return false;
        }
    }
    public boolean handles(Path p) {
        return p.endsWith(extension);
    }
    public static Object readTyped(Path p) {
        if(json.handles(p))
            return json.read(p);
        return yaml.read(p);
    }
    public void addSerializer(Class c, StdSerializer s) {
        var module = new SimpleModule();
        module.addSerializer(c, s);
        fileio.registerModule(module);
    }

    private class tabless extends FilterReader {
        /*
         * This is idiotic.  Many deserializers vomit on TAB \t characters,
         * and yet many data files contain them.  This filter just translates
         * '\t' into ' '
        */
        tabless(Reader in) {
            super(in);
        }
        @Override
        public int read() throws IOException {
            var c = in.read();
            return c == '\t' ? ' ' : c;
        }
        @Override
        public int read(char[] cbuf, int off, int len) throws IOException {
            int rc = 0;
            while(len>0) {
                var e = read();
                if(e<0) return rc;
                cbuf[off++] = (char) e;
                len--;
                rc++;
            }
            return rc;
        }

    }
}

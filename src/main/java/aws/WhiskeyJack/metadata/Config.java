/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.metadata;

import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.nodeviewerfx.*;
import aws.WhiskeyJack.util.*;
import static aws.WhiskeyJack.util.EZOutput.*;
import java.io.*;
import static java.nio.file.FileVisitResult.*;
import java.nio.file.*;
import java.nio.file.attribute.*;
import java.util.*;
import java.util.function.*;
import java.util.regex.*;

public class Config {
    public static final Path configDir = Utils.homePath("." + Graph.graphFileExtension);
    private static final Path propFile = configDir.resolve("config.properties");
    private static final Properties props = new Properties();
    static {
        try(Reader in = Files.newBufferedReader(propFile)) {
            props.load(in);
        } catch(IOException ioe) {
        }
    }
    public static String get(String name, String dflt) {
        return props.getProperty(name, dflt);
    }
    public static boolean get(String name, boolean dflt) {
        return props.getProperty(name, String.valueOf(dflt)).equals("true");
    }
    public static int get(String name, int dflt) {
        return Integer.parseInt(props.getProperty(name, String.valueOf(dflt)));
    }
    public static void put(String name, String value) {
        props.put(name, value);
        sync();
    }
    public static void put(String name, boolean value) {
        props.put(name, String.valueOf(value));
        sync();
    }
    public static void put(String name, int value) {
        props.put(name, String.valueOf(value));
        sync();
    }
    private static void sync() {
        try(var out = CommitableWriter.abandonOnClose(propFile)) {
            props.store(out, "Flowgraph Editor Config");
            out.commit();
        } catch(IOException ioe) {
            Dlg.error("Failed to write to " + propFile, ioe);
        }
    }
    public static Path scanConfig(String kind, BiConsumer<String, String> func) {
        var dir = configDir.resolve(kind);
        var pfxlen = dir.toString().length();
        var work = new TreeMap<String, String>(String.CASE_INSENSITIVE_ORDER);
        var ext = Pattern.compile("/*([^.].*)\\." + kind, Pattern.CASE_INSENSITIVE).matcher("");
        try {
            if(!Files.isDirectory(dir))
                return dir;
            Files.walkFileTree(dir, new FileVisitor<Path>() {
                @Override
                public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                    return CONTINUE;
                }
                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                    return CONTINUE;
                }
                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                    var fn = file.toString();
                    if(ext.reset(fn).matches())
                        work.put(ext.group(1).substring(pfxlen), fn);
                    return CONTINUE;
                }
                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                    return CONTINUE;
                }
            });
            work.forEach((tag, path) -> func.accept(tag, path));
        } catch(IOException ex) {
            Dlg.error("Error walking " + dir, ex);
        }
        return dir;
    }
}

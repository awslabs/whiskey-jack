/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.exl;

import aws.WhiskeyJack.nodegraph.*;
import java.io.*;
import java.nio.file.*;
import static java.nio.file.FileVisitResult.*;
import java.nio.file.attribute.*;
import java.util.*;

public class ExpressionEvolutionCollector {
    private final Path root;
    static private final String codeExt = ".code";
    static private final String diffExt = ".diff";
    private HashMap<Domain,DInfo> prevMap = new HashMap<>();
    private Path current;
    public ExpressionEvolutionCollector(){
        this(Path.of("/tmp/xf"));
    }
    public ExpressionEvolutionCollector(Path r) {
        root = r;
        try {
            if(!Files.exists(root))
                Files.createDirectories(root);
            else
                Files.walkFileTree(root, new FileVisitor<Path>() {
                    @Override
                    public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                        System.out.println("POST: " + dir);
                        return TERMINATE;
                    }
                    @Override
                    public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                        System.out.println("PRE: " + dir);
                        return CONTINUE;
                    }
                    @Override
                    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                        System.out.println("VISIT: " + file);
                        var n = file.toString();
                        if(n.endsWith(diffExt) || n.endsWith(codeExt))
                            Files.deleteIfExists(file);
                        return CONTINUE;
                    }
                    @Override
                    public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                        return CONTINUE;
                    }
                });
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
        }
    }
    public void dump(DomainCode dc) {
        var dinfo = prevMap.computeIfAbsent(dc.domain, k->new DInfo());
        var nm = dinfo.seq<26 ? Character.toString('a'+dinfo.seq) : Integer.toString(dinfo.seq);
        dinfo.seq++;
        current = root.resolve(dc.domain+"-"+nm+codeExt);
        try {
            new VerboseDump().to(current).append(dc).close();
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
        }
        if(dinfo.prev!=null) {
            var diff = current.toString().replace(codeExt, diffExt);
            var pb = new ProcessBuilder( "/bin/sh","-c","exec /usr/bin/diff -u "+dinfo.prev+" "+current+" >"+diff);
            pb.directory(root.toFile());
            try {
                pb.start();
            } catch(IOException ex) {
                ex.printStackTrace(System.out);
            }
        }
        dinfo.prev = current;
    }
    class DInfo {
        Path prev = null;
        int seq = 0;
    }
}

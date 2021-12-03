package com.eastx.sap.batch.extend.item.file.core;

import com.eastx.sap.batch.extend.item.file.infrastructure.Separators;
import com.eastx.sap.batch.extend.item.file.infrastructure.LineMapper;
import com.google.errorprone.annotations.CanIgnoreReturnValue;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.item.file.FlatFileItemReader;
import org.springframework.batch.item.file.FlatFileParseException;
import org.springframework.batch.item.file.NonTransientFlatFileException;
import org.springframework.batch.item.file.ResourceAwareItemReaderItemStream;
import org.springframework.batch.item.support.AbstractItemCountingItemStreamItemReader;
import org.springframework.core.io.Resource;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;

import static com.google.common.base.Preconditions.checkState;

/**
 * @ClassName BinaryFileItemReader
 * @Description:
 *
 *  (a) 调用栈
 *      ItemReader.doRead 调用栈
 *      return this.lineMapper.mapLine(line, this.lineNumber);
 *          return this.fieldSetMapper.mapFieldSet(this.tokenizer.tokenize(line));  ---
 *              (1) public FieldSet tokenize(@Nullable String line)
 *              (2) this.fieldSetMapper.mapFieldSet
 *                  fs.getProperties()
 *                  DataBinder
 *                  return T
 *  (b) 数据对象
 *      FieldSet : 保存原始的数据拆分和定位
 *      FieldSetMapper : new Object, convert, set; FieldSet -> T
 *      LineMapper : 整合 FieldSet 和 FieldSetMapper; String -> T
 *      LineTokenizer : String -> FieldSet
 *
 *      优点：非常灵活，结构容易扩展
 *      缺点1： 配置太多，而且没有完全根据传入的target.class进行分析自动适配FieldSetMapper
 *      缺点2: 默认使用的是String进行数据传输
 *
 *  (c) 思考通达信这种，二进制，定长，按照字节来token Field
 *
 *      保持 FieldSet FieldSetMapper LineMapper 三层结构
 *      对传入的数据进行抽象，参考Stream Spliterator
 *
 * @Author Tender
 * @Time 2021/8/1 0:21
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
public class BinaryFileItemReader<T> extends AbstractItemCountingItemStreamItemReader<T>
        implements ResourceAwareItemReaderItemStream<T> {
    /**
     * The file reader
     */
    private BufferedInputStream in;

    /**
     * Map a line to a object,important~
     */
    private LineMapper<T> lineMapper;

    /**
     * How many lines we have read
     */
    private int lineNumber = 0;

    /**
     * Is the reader working or not
     */
    private boolean notWorking = false;

    /**
     * The resource we want to process
     */
    private Resource resource;

    /**
     * Skip the lines before processing
     */
    private int linesToSkip = 0;

    /**
     *
     */
    private final int chunkSize;

    /**
     *
     */
    private byte chunkBuff[];

    /**
     *
     * @param chunkSize
     */
    public BinaryFileItemReader(int chunkSize) {
        //set a default name to avoid exception in taskExecution
        setName(ClassUtils.getShortName(FlatFileItemReader.class));
        this.chunkSize = chunkSize;
        this.chunkBuff = new byte[chunkSize];
    }

    @Override
    public void setResource(Resource resource) {
        this.resource = resource;
    }

    public void setLinesToSkip(int linesToSkip) {
        this.linesToSkip = linesToSkip;
    }

    public void setLineMapper(LineMapper<T> lineMapper) {
        this.lineMapper = lineMapper;
    }

    /**
     * @return
     * @throws Exception
     */
    @Nullable
    @Override
    public T doRead() {
        if (this.notWorking) {
            return null;
        } else {
            byte[] line = readLine();
            if (line == null) {
                return null;
            } else {
                try {
                    return this.lineMapper.mapLine(Separators.separator(line), this.lineNumber);
                } catch (Exception e) {
                    throw new FlatFileParseException("Parsing error at line: " + this.lineNumber + " in " + this.resource.getDescription(), e, line.toString(), this.lineNumber);
                }
            }
        }
    }

    @Override
    public void doOpen() throws Exception {
        Assert.notNull(this.resource, "Input resource must be set");
        this.notWorking = true;

        //check the resource
        checkState(resource.exists(), "The resource not exists, %s", this.resource);
        checkState(resource.isReadable(), "The resource can be read, %s", this.resource);

        //open the input stream
        this.in = new BufferedInputStream(resource.getInputStream());

        //Let us working now
        this.notWorking = false;
    }

    @Override
    public void doClose() throws Exception {
        this.lineNumber = 0;
        if (this.in != null) {
            this.in.close();
        }
    }

    @Nullable
    private byte[] readLine() {
        if (this.in == null) {
            throw new IllegalStateException("The input stream must be opened before reading a line");
        } else {
            try {
                //如果数据和buff块大小对不上则提前结束
                if(tryReadChunk()){
                    ++this.lineNumber;
                    return chunkBuff;
                } else {
                    //如果最后剩余字节不满足一个chunkSize则认为终止
                    return null;
                }
            } catch (IOException e) {
                this.notWorking = true;
                throw new NonTransientFlatFileException("Unable to read from resource: [" + this.resource + "]", e, "line", this.lineNumber);
            }
        }
    }

    private boolean tryReadChunk() throws IOException {
        return readChunk(in, chunkBuff, 0 , chunkSize) == chunkSize;
    }

    /**
     * Copy from guava
     *
     * @param in
     * @param b
     * @param off
     * @param len
     * @return the total bytes read,if 0 then is end
     * @throws IOException
     */
    @CanIgnoreReturnValue
    public static int readChunk(InputStream in, byte[] b, int off, int len) throws IOException {
        if (len < 0) {
            throw new IndexOutOfBoundsException("len is negative");
        } else {
            int total;
            int result;
            for(total = 0; total < len; total += result) {
                result = in.read(b, off + total, len - total);
                if (result == -1) {
                    break;
                }
            }

            return total;
        }
    }
}

package com.eastx.sap.controller;

import com.eastx.sap.error.ErrorEnum;
import com.eastx.sap.error.ExceptionFactory;
import com.eastx.sap.data.pojo.Stock;
import com.eastx.sap.server.StockServer;
import com.eastx.sap.service.StockService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;
import java.util.concurrent.Future;

/**
 * @ClassName StockController
 * @Description: 提供WebSocket Server配套的Controller
 * @Author Tender
 * @Time 2021/7/11 11:18
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
@RestController
@RequestMapping("stock")
public class StockController {
    @Autowired
    ExceptionFactory exceptionFactory;

    @Autowired
    StockService stockService;

    /**
     * 确保只执行一次异步线程
     */
    private static Future<String> stockFuture;

    /**
     * 群发消息内容
     * @return
     */
    @RequestMapping(value="flow", method= RequestMethod.GET)
    public void flow(@RequestParam(required=false) String id){
        if(Optional.ofNullable(stockFuture).map(f->f.isDone()).orElse(true)) {
            stockFuture = stockService.supplyFlow(StockServer::broadcastBegin, StockServer::broadcast, StockServer::broadcastEnd);
        } else {
            throw exceptionFactory.newException(ErrorEnum.ASYNC_RUNNING);
        }
    }

    /**
     * 群发消息内容
     * @return
     */
    @RequestMapping(value="/once", method= RequestMethod.GET)
    public Stock once(@RequestParam(required=false) String id) {
        return Stock.valueOf("姚林", stockService.supplyOnce());
    }

}

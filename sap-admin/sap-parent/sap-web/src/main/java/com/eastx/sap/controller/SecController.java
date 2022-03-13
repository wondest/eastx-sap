package com.eastx.sap.controller;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

/**
 * @ClassName SecController
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/2 23:59
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
@RestController
@RequestMapping("sec")
public class SecController {

    /**
     * 群发消息内容
     * @return
     */
    @RequestMapping(value="/product/{id}", method= RequestMethod.GET)
    public void getProduct(@PathVariable String id){
        log.info("get product");
        log.info(id);
    }

    /**
     * 群发消息内容
     * @return
     */
    @RequestMapping(value="/user/{id}", method= RequestMethod.GET)
    public void getUser(@PathVariable String id){
        log.info("get user");
        log.info(id);
    }
}

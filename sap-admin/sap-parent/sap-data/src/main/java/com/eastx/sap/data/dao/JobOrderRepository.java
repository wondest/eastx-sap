package com.eastx.sap.data.dao;

import com.eastx.sap.data.entity.JobOrder;
import com.eastx.sap.data.type.ExecLockEnum;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

/**
 * @ClassName JobOrderRepository
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/7 0:01
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface JobOrderRepository extends JpaRepository<JobOrder, String> {
    @Query("update #{#entityName} set execId = ?3, lockState = ?4 where id = ?1 and execState = ?2")
    int updateWithExecState(String id, ExecLockEnum execState, Long execId, ExecLockEnum expectedExecState);
}

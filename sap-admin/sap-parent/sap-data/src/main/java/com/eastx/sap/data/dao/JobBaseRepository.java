package com.eastx.sap.data.dao;

import com.eastx.sap.data.entity.JobBase;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * @ClassName JobBaseRepository
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/7 0:01
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface JobBaseRepository extends JpaRepository<JobBase, String> {
//    /**
//     *
//     * @param jobName
//     * @return
//     */
//    @Query("select t from #{#entityName} t where t.jobName = ?1")
//    List<JobBase> findByJobName(String jobName);
//
//    /**
//     *
//     * @param jobName
//     * @param pageable
//     * @return
//     */
//    @Query(value = "select t from #{#entityName} t where t.jobName = ?1",
//            countQuery = "select count(*) from #{#entityName} t where t.jobName = ?1",
//            nativeQuery = true)
//    Page<JobBase> findByJobName(String jobName, Pageable pageable);
}

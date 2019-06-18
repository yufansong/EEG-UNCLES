% initial the variable
all_time = 4000;
k=1;
for order=1:4
    % load file
    if (order == 1)
        file_order = '1_1';
    elseif(order == 2)    
        file_order = '1_2';
    elseif (order == 3)
        file_order = '2_1';
    else 
        file_order = '2_2';
    end
    filename = ['onedata_deal_',file_order,'.csv'];
    M = csvread(filename,1,1);
    [m,n] = size(M);

    % deal with data
    cluster = zeros(m,all_time);
    for clu_num = 1:m
        result = zeros(1,1,all_time);
        for j =1:128
            if( M(clu_num,j)==1 )
                for i =1:80
                        result = result + onedata(k,j,:,i);
                end
                % for person_number = 1:9 10:20
                %     data_person=['CLAS_VP',mat2str(person_number),'_onedata_STBFH_MNT.mat'];
                %     load(data_person);
                %     for i =1:80
                %         result = result + onedata(k,j,:,i);
                %     end
                % end
            end
        end
        cluster(clu_num,:) = reshape(result,1,all_time)
    end

    % plot and save picture
    cluster = cluster'
    plot(cluster);
    save_name = [file_order,'_cluster_all.png']
    print(gcf,'-dpng',save_name)
    for clu_num = 1:m
        plot(cluster(:,clu_num));
        save_name = [file_order,'cluster_',mat2str(clu_num),'.png']
        print(gcf,'-dpng',save_name)
    end
end
% initial the variable
clear
clc
all_frequency = 400;
k=1;
for order=1:1
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
    cluster = zeros(m,all_frequency);
    for clu_num = 1:m
        result = zeros(1,all_frequency);
        for j =1:128
            if( M(clu_num,j)==1 )
                for person_number = 1:20
                    filename=['onedata_',mat2str(person_number),'_',file_order,'.csv'];
                    data = csvread(filename);
                    result = result + data(j,:);
                end
            end
        end
        cluster(clu_num,:) = reshape(result,1,all_frequency)
    end

    % plot and save picture
    cluster = cluster'
    plot(cluster);
    save_name = [file_order,'_cluster_all.png']
    print(gcf,'-dpng',save_name)
    for clu_num = 1:m
        plot(cluster(:,clu_num),'--');
        save_name = [file_order,'cluster_',mat2str(clu_num),'.png']
        print(gcf,'-dpng',save_name)
    end
end
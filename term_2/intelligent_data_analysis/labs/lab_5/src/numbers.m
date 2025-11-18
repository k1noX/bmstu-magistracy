function numbers()
    % Определение функции чтения изображений из файла
    function y = ImgRead(x)
        img = imread(x);
        
        if size(img, 3) == 3
            img = rgb2gray(img);
        end
        
        [rows, cols] = size(img);
        total_pixels = rows * cols;
        
        img1 = img';
        img2 = reshape(img1, [total_pixels, 1]);
        
        for j = 1:total_pixels
            if img2(j,1) <= 125
                img2(j,1) = 1;
            end
        end
        
        for j = 1:total_pixels
            if img2(j,1) > 125
                img2(j,1) = 0;
            end
        end
        
        y = img2;
    end

    clc; clear;

    %% Обучение модели
    images = cell(10, 1);
    sizes = [];
    for i = 1:10
        letter = char(47 + i);
        filename = sprintf('..\\data\\numbers\\%s.png', letter);
        images{i} = ImgRead(filename); % Сохраняем в ячейку
        sizes = [sizes; numel(images{i})];
    end
 
    if length(unique(sizes)) > 1
        error('Все изображения должны иметь одинаковый размер!');
    end
  
    RA = [];
    for i = 1:10
        RA = [RA, images{i}]; % Горизонтальная конкатенация столбцов
    end

    P = double(RA); 
    T = eye(10);
    [R, Q] = size(P);
    [~, S2] = size(T);

    % Создаем зашумленные данные
    P1 = P; 
    T1 = T;
    for i = 1:100
        noise1 = rand(R, Q) * 0.1;
        noise2 = rand(R, Q) * 0.2;
        P1 = [P1, P + noise1, P + noise2];
        T1 = [T1, T, T];
    end

    noisy5 = P(:, 5) + randn(R, 1) * 0.2;

    save('numbers_mat.mat', 'P', 'T', 'P1', 'T1', 'noisy5');
 
    %% Обучение модели
    nnstart;

    %% Построение зашумлённого изображения
    noisy5 = P(:, 5) + randn(160, 1) * 0.2;
    imagesc(reshape(noisy5, [10,16])'), colormap(gray), axis image, axis off, title(char(47+5));
    %% 
    output = sim(results_6.Network, noisy5); % net - экспортированная обученная сеть
    
    % Преобразование выхода в читаемый результат
    result = compet(output);
    [~, answer] = max(result);
    fprintf('Распознанный символ: %s\n', char(47 + answer));
    imagesc(reshape(P(:, answer), [10,16])'), colormap(gray), axis image, axis off, title(char(47 + answer));
end
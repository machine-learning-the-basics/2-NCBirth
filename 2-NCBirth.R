# === 1. Подключение и подготовка данных ===

library(Stat2Data)  # Загрузка пакета с данными
data('NCbirths')    # Загрузка набора данных NCbirths

# Преобразование переменной количества детей в фактор с уровнями "1", "2", "3"
NCbirths$Plural = factor(NCbirths$Plural, levels = c("1", "2", "3"))

# Преобразование пола в фактор с уровнями Male и Female
NCbirths$Sex = factor(NCbirths$Sex)
levels(NCbirths$Sex) = c("Male", "Female")

# Преобразование семейного положения в фактор с уровнями yes и no
NCbirths$Marital = factor(NCbirths$Marital)
levels(NCbirths$Marital) = c("yes", "no")

# Преобразование расы матери в фактор с наименованиями рас
NCbirths$RaceMom = factor(NCbirths$RaceMom)
levels(NCbirths$RaceMom) = c('white', 'black', 'american indian','chinese','japanese', 'hawaiian', 'filipino', 'other asian or pacific islander')
NCbirths$RaceMom = as.factor(NCbirths$RaceMom)  # Повторное преобразование в фактор на всякий случай

# Преобразование принадлежности к испаноязычной группе
NCbirths$HispMom = factor(NCbirths$HispMom)
levels(NCbirths$HispMom) = c("Cuban", "Mexican", "not Hispanic", "Other Hispanic", "Puerto Rico", "Central/South America")

# Преобразование признаков: курение, недоношенность, низкий вес
NCbirths$Smoke = factor(NCbirths$Smoke)
levels(NCbirths$Smoke) = c("no", "yes")
NCbirths$Low = factor(NCbirths$Low)
levels(NCbirths$Low) = c("no", "yes")
NCbirths$Premie = factor(NCbirths$Premie)
levels(NCbirths$Premie) = c("no", "yes")


# === 2. Сводная информация о данных ===

summary(NCbirths)  # Вывод общей статистики по всем переменным


# === 3. Диаграмма рассеяния: вес новорожденного vs возраст матери ===

plot(NCbirths$BirthWeightGm, NCbirths$MomAge, 
     xlab = "Вес новорожденного", 
     ylab = "Возраст матери", 
     col = "darkblue")  # Простая scatter plot


# === 4. Гистограмма и плотность для подгруппы: белые курящие матери ===

library(ggplot2)  # Подключение ggplot2

subset_data = subset(NCbirths, RaceMom == "white" & Smoke == "yes")  # Выборка подгруппы

# Гистограмма с наложенной кривой плотности
ggplot(subset_data, aes(x = BirthWeightGm)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, 
                 fill = "lightblue", color = "blue", alpha = 0.7) +
  geom_density(color = "darkblue", linewidth = 1) +
  labs(
    title = "Распределение веса новорожденных, матери которых относятся к белой расе и курят",
    x = "Вес ребенка (граммы)",
    y = "Относительная частота"
  ) +
  theme_classic()


# === 5. Диаграмма размаха веса в зависимости от количества детей (Plural) ===

ggplot(NCbirths, aes(x = factor(Plural), y = BirthWeightGm)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(
    title = "Диаграммы размаха веса новорожденных в зависимости от количества детей",
    x = "Количество детей",
    y = "Вес ребенка (граммы)"
  ) +
  theme_classic()


# === 6. Средний вес новорожденных по группам количества детей ===

library(dplyr)  # Для обработки и группировки данных

# Группировка по количеству детей и расчет среднего веса
mean_weights = NCbirths %>%
  group_by(Plural) %>%
  summarise(mean_weight = mean(BirthWeightGm, na.rm = TRUE))

# Построение столбиковой диаграммы со средним весом
ggplot(mean_weights, aes(x = factor(Plural), y = mean_weight)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "blue") +
  labs(
    title = "Средний вес новорожденных при разных значениях Plural",
    x = "Количество детей (Plural)",
    y = "Средний вес (граммы)"
  ) +
  theme_classic()


# === 7. Частотный анализ категориальных переменных и мозаичная диаграмма ===

# Создание таблицы сопряженности по Smoke, Premie и Low
contingency_table <- table(NCbirths$Smoke, NCbirths$Premie, NCbirths$Low)
print(contingency_table)

library(vcd)  # Для построения мозаичных диаграмм

# Построение мозаичной диаграммы: отображение связи между тремя переменными
mosaic(~Smoke+Premie+Low, data = NCbirths, shade=TRUE, 
       main = "Мозаичная диаграмма: Smoke, Premie, Low")


# === 8. Матричные диаграммы рассеяния + регрессия ===

library(psych)  # Для функции pairs.panels

# Диаграммы рассеяния, гистограммы и линии регрессии
pairs.panels(NCbirths[, c("MomAge", "BirthWeightGm", "Weeks")],
             hist.col = 'lightblue', 
             main = "Матричные диаграммы рассеяния: MomAge, BirthWeightGm, Weeks",
             lm = TRUE)  # Добавление линии регрессии


# === 9. Диаграмма Кливленда для новорожденных из двоен (Twins) ===

# Фильтрация данных: только двойни
twins_data <- NCbirths %>% filter(Plural == "2")

# Создание возрастных групп
twins_data = twins_data %>%
  mutate(AgeGroup = case_when(
    MomAge < 25 ~ "Моложе 25 лет",
    MomAge >= 25 & MomAge <= 35 ~ "От 25 до 35 лет",
    MomAge > 35 ~ "Старше 35 лет"
  ))

# Сортировка по весу ребенка
twins_data = twins_data %>%
  arrange(BirthWeightGm)

# Диаграмма Кливленда: вес по наблюдениям, цвет — возраст матери
ggplot(twins_data, aes(x = reorder(rownames(twins_data), BirthWeightGm), 
                       y = BirthWeightGm, 
                       color = AgeGroup)) +
  geom_point(shape = 1, size = 3, stroke = 1.5) +  # Пустые кружки
  scale_color_manual(values = c("Моложе 25 лет" = "lightblue", 
                                "От 25 до 35 лет" = "blue", 
                                "Старше 35 лет" = "darkblue")) +
  labs(title = "Диаграмма Кливленда: Вес новорожденных из двоен",
       x = "Наблюдения (упорядоченные по весу)",
       y = "Вес новорожденных (граммы)",
       color = "Возраст матери") + 
  theme_minimal()


# === 10. Диаграмма с областями: пропорция пола от веса при рождении ===

# Подготовка данных: подсчёт количества и долей новорожденных по полу и весу
birth_data = NCbirths %>%
  group_by(Sex, BirthWeightGm) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(BirthWeightGm) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Построение area chart — отображение пропорций по полу
ggplot(birth_data, aes(x = BirthWeightGm, y = prop, fill = Sex)) +
  geom_area(alpha = 0.6, position = 'fill') +  # Пропорции по полу
  labs(title = "Пропорции новорожденных разного пола от веса при рождении",
       x = "Вес при рождении (граммы)",
       y = "Пропорция") +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "darkblue")) +
  theme_minimal() +
  theme(legend.title = element_blank())

doubleUs x y = x*2 + y*2
doubleMe x = x + x
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
conan'Obrien = "hello"
neural x y = 1 * x + 0 * y + 1
sigmoid x = 1/(1 + exp (-x))
neural' x1 x2 x3 x4 = 0.2*x1 + 0.4*x2 + 0.6*x3 + 0.8*x4 + 1
epsilon_exit w y_j = w + 0.5 * y_j * 0.933 * (1 - 0.933) * (0 - 0.933)
hidden_epsilon w y_j = w + 0.5 * 0 * w * y_j * (1 - y_j) * 0.933 * (1 - 0.933) * (0 - 0.933)

